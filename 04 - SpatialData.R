library(rgeos)
library(sp)
library(rgdal)

#Load the data
load("Data\\DadosGWR.RData")
rm(list=setdiff(ls(), "dados"))

#Features
real.df <- dados[,c("Longitude","Latitude","VLR_PACTUA","Tempo")]
real.df$Longitude <- as.numeric(as.character(real.df$Longitude))
real.df$Latitude <- as.numeric(as.character(real.df$Latitude))
#Read map
map <- readOGR("Malhas","53SEE250GC_SIR",verbose = FALSE)
proj4string(map) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Assignment modified according
real.spdf <- SpatialPointsDataFrame(real.df[,c("Longitude", "Latitude")], real.df[,c("Longitude","Latitude","VLR_PACTUA","Tempo")])
proj4string(real.spdf) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(real.spdf) <- proj4string(map)

real.df<-na.omit(real.df)
real.df$day_number <- as.numeric(difftime(real.df$Tempo, min(real.df$Tempo),units="days"))
real.df<-real.df[,-4]
real.mat <- as.matrix(real.df)

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

#Normalize
real.mat<-apply(real.mat,2,as.numeric)
real.mat<-apply(real.mat,2,function(x) (x-min(x))/(max(x)-min(x)))

#Sample
ind <- sample(2, nrow(real.mat), replace=TRUE, prob=c(0.67, 0.33))

# Split the data
real.training <- real.mat[ind==1, c(1,2,4)]
real.test <- real.mat[ind==2,  c(1,2,4)]

# Split the class attribute
real.trainingtarget <- as.numeric(real.mat[ind==1, 3])
real.testtarget <- as.numeric(real.mat[ind==2, 3])

#Begin the model
model <- keras_model_sequential() 

#Architeture
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = 3) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'softmax')

# Compile the model
model %>% compile(optimizer='adam',
                  loss='mse')

# Fit the model 
history <- model %>% fit(
  real.training, 
  real.trainingtarget, 
  epochs = 200, 
  batch_size = 10, 
  validation_split = 0.2
)

plot(history)


#Evaluate
score <- model %>% evaluate(real.test, real.testtarget, batch_size = 128)
sqrt(score) #rmse

### Make Submission
model %>% fit(real.training, real.trainingtarget, epochs=100, batch_size=128)

#Prediction
y <- predict(model, real.test)

