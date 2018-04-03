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

#Load the data
load("Data\\FinalData.RData")

real.mat <- as.matrix(final[,c(-1,-2,-3)])

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

#Normalize
real.mat<-apply(real.mat,2,as.numeric)
real.mat<-apply(real.mat,2,function(x) (x-min(x))/(max(x)-min(x)))

design<-real.mat[,c(-28,-54)]
target<-real.mat[,28]

#Begin the model
model <- keras_model_sequential() 

#Architeture
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = 52) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'softmax')

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


#Evaluate
score <- model %>% evaluate(real.test, real.testtarget, batch_size = 128)
sqrt(score) #rmse

### Make Submission
model %>% fit(real.training, real.trainingtarget, epochs=100, batch_size=128)

#Prediction
y <- predict(model, real.test)

