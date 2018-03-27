library(keras)
library(reticulate)
library(foreign)
library(dplyr)
library(lubridate)

#https://keras.io/getting-started/sequential-model-guide/
#https://machinelearningmastery.com/time-series-prediction-lstm-recurrent-neural-networks-python-keras/
#https://blog.rstudio.com/2017/09/05/keras-for-r/

#Load the data
load("Data\\DadosGWR.RData")
rm(list=setdiff(ls(), "dados"))

#Features
real.df <- dados[,c("Longitude","Latitude","VLR_PACTUA","Tempo")]
real.df<-na.omit(real.df)
real.df$day_number <- as.numeric(difftime(real.df$Tempo, min(real.df$Tempo),units="days"))
real.df<-real.df[,-4]
real.mat <- as.matrix(real.df)

# Set `dimnames` to `NULL`
dimnames(real.mat) <- NULL

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
model %>% compile(optimizer='rmsprop',
                  loss='mse',
                   optimizer='adam')

# Fit the model 
history <- model %>% fit(
  real.training, 
  real.trainingtarget, 
  epochs = 10, 
  batch_size = 5, 
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

