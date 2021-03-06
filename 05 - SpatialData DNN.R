library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
library(lubridate)
library(zoo)
library(geosphere)
library(keras)
library(reticulate)
library(foreign)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(rgeos)
library(RgoogleMaps)
library(spdep)
library(ggmap)
library(plyr)
library(Hmisc)
library(gstat)
#Load the data
load("Data\\FinalData.RData")

real.mat <- as.matrix(final[,c(-1,-2,-3)])

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

#Normalize
real.mat<-apply(real.mat,2,as.numeric)
#real.mat<-apply(real.mat,2,function(x) (x-min(x))/(max(x)-min(x)))

design<-real.mat[,c(-171,-342)]
target<-real.mat[,171]

#Begin the model
model <- keras_model_sequential() 

#Architeture
model %>% 
  layer_dense(units = 256, activation = 'tanh', input_shape = 340) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'tanh') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'tanh')

# Compile the model
model %>% compile(optimizer='adam',
                  loss='mse')

# Fit the model 
history <- model %>% fit(
  design, 
  target, 
  epochs = 1000, 
  batch_size = 100, 
  validation_split = 0.5
)

plot(history)

dataGraph<-as.data.frame(history$metrics)
dataGraph <- reshape2::melt(dataGraph)
dataGraph$Spline<-ifelse(dataGraph$variable=="loss", "Training","Validation")
#dataGraph$value<- 1e+10*dataGraph$value
dataGraph$Epoch<-c(seq(1,nrow(as.data.frame(history$metrics))),seq(1,nrow(as.data.frame(history$metrics))))
#quantile(dataGraph$value,probs=0.90)
#idsRemove<-dataGraph$Epoch[which(dataGraph$value>4e+5)]
#dataGraph<-dataGraph[-which(dataGraph$Epoch%in%idsRemove),]

p <- ggplot(dataGraph, aes(Epoch, value))
p<- p + geom_point(aes(colour = factor(Spline)), size = 1)
#p<- p +  geom_smooth(method=lm,formula = y ~ splines::bs(x, 3), aes(fill= Spline))
p<- p+  labs(
  x = "Epoch",
  y = "Loss value",
  colour = "Groups"
)
p
ggsave(filename="SplineTraining.pdf", plot=p, width = 210, height = 297, units = "mm")

####Generate Forecast (Time 1: 2014-03-02):
n.ahead<-100
design.for<-design[,1:170]
forecast<-data.frame("TIME1"=target)
for(i in 2:n.ahead){
  #Step 1: remove the first period
  design.for<-design.for[,-1]
  #Step 2: Add the last results
  design.for<-cbind(design.for,target)
  #Step 3: Create the neighborhood prices
  x<-weight.Mat%*%design.for
  design.full<-cbind(design.for,x)
  #Step 4: Forecast the new values
  y <- predict(model, design.full)
  forecast[,i]<-y
  colnames(forecast)[i]<-paste0("TIME",i)
}
colnames(forecast)<-seq(as.Date("2014/3/2"), by = "month", length.out = n.ahead)

#Create the map
forecast<-cbind(setor.map[,1:3],forecast)
forecast$CD_GEOCODI<-as.character(forecast$CD_GEOCODI)

#Read the map
sfn <- readOGR("Malhas","53SEE250GC_SIR",verbose = FALSE) 

b <- bbox(sfn)
#ID variable
sfn@data$id <- rownames(sfn@data)

#Projection
sfn <- spTransform(sfn, CRS("+proj=longlat +datum=WGS84"))

#GGPLOt2 Transformation
sfn.df <- fortify(sfn, region="CD_GEOCODI")

#Merge with all CD_GEOCODI
trueCentroids <- as.data.frame(gCentroid(sfn,byid=TRUE))
sfn.data<-cbind(trueCentroids,sfn@data)
forecast<-merge(forecast,sfn.data,by="CD_GEOCODI",all=T)
forecast<-forecast[,c(-2,-3)]
colnames(forecast)[c(102,103)]<-c("Longitude","Latitude")
colnames(forecast)[c(2,101)]<-c("Begin","End")
forecast<-as.data.frame(forecast)
#Interpolate data
idNA<-is.na(forecast$Begin)
forecast.clean<-forecast[!idNA,]
coordinates(forecast.clean) = ~Longitude + Latitude
forecast.empty<-forecast[idNA,]
coordinates(forecast.empty) = ~Longitude + Latitude
idw <- idw(formula = End ~ 1, locations = forecast.clean, newdata = forecast.empty)
forecast.empty$End<-idw$var1.pred
idw <- idw(formula = Begin ~ 1, locations = forecast.clean, newdata = forecast.empty)
forecast.empty$Begin<-idw$var1.pred
forecast.final<-rbind(forecast.clean@data,forecast.empty@data)

#Get the data.frame
forecast<-forecast.final
forecast<-forecast[,1:101]
forecast$Return<-round(100*((forecast$End-forecast$Begin)/forecast$Begin),2)

#Combine
sfn.df<-merge(sfn.df, forecast,by.x="id", by.y="CD_GEOCODI",all.x=T)

#Ordena os dados
sfn.df<-sfn.df[order(sfn.df$order), ] 
bbox <- ggmap::make_bbox(sfn.df$long, sfn.df$lat, f = 0.1)

#Colors
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#Google maps
map2 <- get_map(location=bbox, source='google', maptype = 'satellite')

#Constrói o mapa:
map2 <- ggmap(map2, base_layer=ggplot(data=sfn.df, aes(x=long, y=lat)), 
             extent = "normal", maprange=FALSE)
map2 <- map2 + geom_polygon(data=sfn.df,aes(x = long, y = lat, group = group, fill=End), alpha = .6)  
map2 <- map2 +   geom_path(aes(x = long, y = lat, group = group),
                         data = sfn.df, colour = NA, alpha = .7, size = .4, linetype=2)  
map2 <- map2 + coord_equal() 
map2 <- map2 + scale_fill_gradientn(colours = myPalette(4), na.value = "transparent",name = "Normalized\nPrice")
map2<-map2 +  ggtitle("") +  labs(x="Longitude",y="Latitude") 
#Plota o mapa
plot(map2)
ggsave(filename="MS2022-06-02.pdf", plot=map2, width = 210, height = 297, units = "mm")

bbox <- ggmap::make_bbox(sfn.df$long, sfn.df$lat, f = 0.1)

#Colors
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#Google maps
map3 <- get_map(location=bbox, source='google', maptype = 'satellite')

#Constrói o mapa:
map3 <- ggmap(map3, base_layer=ggplot(data=sfn.df, aes(x=long, y=lat)), 
              extent = "normal", maprange=FALSE)
map3 <- map3 + geom_polygon(data=sfn.df,aes(x = long, y = lat, group = group, fill=Begin), alpha = .6)  
map3 <- map3 +   geom_path(aes(x = long, y = lat, group = group),
                           data = sfn.df, colour = NA, alpha = .7, size = .4, linetype=2)  
map3 <- map3 + coord_equal() 
map3 <- map3 + scale_fill_gradientn(colours = myPalette(4), na.value = "transparent",name = "Normalized\nPrice")
map3<-map3 +  ggtitle("") +  labs(x="Longitude",y="Latitude") 
#Plota o mapa
plot(map3)
ggsave(filename="MS2014-03-02.pdf", plot=map3, width = 210, height = 297, units = "mm")


bbox <- ggmap::make_bbox(sfn.df$long, sfn.df$lat, f = 0.1)

#Colors
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#Google maps
map4 <- get_map(location=bbox, source='google', maptype = 'satellite')

#Constrói o mapa:
map4 <- ggmap(map4, base_layer=ggplot(data=sfn.df, aes(x=long, y=lat)), 
              extent = "normal", maprange=FALSE)
map4 <- map4 + geom_polygon(data=sfn.df,aes(x = long, y = lat, group = group, fill=Return), alpha = .6)  
map4 <- map4 +   geom_path(aes(x = long, y = lat, group = group),
                           data = sfn.df, colour = NA, alpha = .7, size = .4, linetype=2)  
map4 <- map4 + coord_equal() 
map4 <- map4 + scale_fill_gradientn(colours = myPalette(4), na.value = "transparent",name = "Return rate")
map4<-map4 +  ggtitle("") +  labs(x="Longitude",y="Latitude") 
#Plota o mapa
plot(map4)
ggsave(filename="MS-Return.pdf", plot=map4, width = 210, height = 297, units = "mm")
