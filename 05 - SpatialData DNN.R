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

#Load the data
load("Data\\FinalData.RData")

real.mat <- as.matrix(final[,c(-1,-2,-3)])

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

#Normalize
real.mat<-apply(real.mat,2,as.numeric)
#real.mat<-apply(real.mat,2,function(x) (x-min(x))/(max(x)-min(x)))

design<-real.mat[,c(-28,-54)]
target<-real.mat[,28]

#Begin the model
model <- keras_model_sequential() 

#Architeture
model %>% 
  layer_dense(units = 256, activation = 'tanh', input_shape = 52) %>% 
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
  epochs = 200, 
  batch_size = 10, 
  validation_split = 0.2
)

plot(history)

####Generate Forecast (Time 1: 2014-03-02):
n.ahead<-100
design.for<-design[,1:26]
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

#Combine
sfn.df<-merge(sfn.df, forecast,by.x="id", by.y="CD_GEOCODI",all.x=T)

#Ordena os dados
sfn.df<-sfn.df[order(sfn.df$order), ] 
bbox <- ggmap::make_bbox(sfn.df$long, sfn.df$lat, f = 0.1)

#Colors
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#Google maps
map <- get_map(location=bbox, source='google', maptype = 'terrain', color='bw')

#Constrói o mapa:
map <- ggmap(map, base_layer=ggplot(data=sfn.df, aes(x=long, y=lat)), 
             extent = "normal", maprange=FALSE)
map <- map + geom_polygon(data=sfn.df,aes(x = long, y = lat, group = group, fill=`2022-06-02`), alpha = .6)  
map <- map +   geom_path(aes(x = long, y = lat, group = group),
                         data = sfn.df, colour = "grey50", alpha = .7, size = .4, linetype=2)  
map <- map + coord_equal() 
map <- map + scale_fill_gradientn(colours = myPalette(4))
map<-map +  ggtitle("Housevalue predict") +  labs(x="Longitude",y="Latitude") 
#Plota o mapa
plot(map)


#Google maps
sfn.df2<-na.omit(sfn.df)
map2 <- get_map(location=bbox, source='google', maptype = 'satellite')

#Constrói o mapa:
map2 <- ggmap(map2, base_layer=ggplot(data=sfn.df2, aes(x=long, y=lat)), 
             extent = "normal", maprange=FALSE)
map2 <- map2 + geom_polygon(data=sfn.df2,aes(x = long, y = lat, group = group, fill=`2022-06-02`), alpha = .6)  
map2 <- map2 +   geom_path(aes(x = long, y = lat, group = group),
                         data = sfn.df2, colour = NA, alpha = .7, size = .4, linetype=2)  
map2 <- map2 + coord_equal() 
map2 <- map2 + scale_fill_gradientn(colours = myPalette(4))
map2<-map2 +  ggtitle("Housevalue predict") +  labs(x="Longitude",y="Latitude") 
#Plota o mapa
plot(map2)
