library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
library(lubridate)
library(zoo)
library(geosphere)
#Load the data
load("Data\\DadosGWR.RData")
rm(list=setdiff(ls(), "dados"))
dadosCoord<-read.csv("Data\\Lote_siturb_padronizado.csv")
dadosCoord<-dadosCoord[,c("Longitude","Latitude","Longitude_padrao","Latitude_padrao")]
#Features
real.df <- dados[,c("Longitude","Latitude","ValorM2","Tempo")]
real.df$Longitude <- as.numeric(as.character(real.df$Longitude))
real.df$Latitude <- as.numeric(as.character(real.df$Latitude))
#Merge new Latitude
final<- inner_join(dadosCoord,real.df,by=c("Longitude","Latitude")) %>% 
        select(-Longitude,-Latitude)

#Read map
map <- readOGR("Malhas","53SEE250GC_SIR",verbose = FALSE)

# Assignment modified according
real.spdf <- SpatialPointsDataFrame(final[,c("Longitude_padrao", "Latitude_padrao")], final[,c("Longitude_padrao","Latitude_padrao","ValorM2","Tempo")])

#Find Setor Censitario
proj4string(real.spdf) <- proj4string(map)
setor<-over(real.spdf, map)
real.spdf@data<-cbind(setor,real.spdf@data)

#Number of setor cenistarios
count <- real.spdf@data %>% 
  group_by(CD_GEOCODI, Date=floor_date(Tempo, "month")) %>% 
  filter(Tempo > "2000-01-01") %>% 
  summarize(amount=median(ValorM2))

#Create Sequence
count<- merge(
  x = data.frame(
    Date = seq.Date(min(count$Date), max(count$Date), by = "month")
  ),
  y = count,
  all.x = TRUE
)
#Remove NA
count <- count[!is.na(count$CD_GEOCODI),]
long <- count %>% spread(Date, amount)

#Neighborhood matrix
map.temp <- unique(real.spdf@data[,c("CD_GEOCODI","Longitude_padrao","Latitude_padrao")])
map.temp <- map.temp %>% 
            group_by(CD_GEOCODI) %>% 
            summarise(Longitude_padrao=mean(Longitude_padrao),
                      Latitude_padrao=mean(Latitude_padrao))
setor.map <- map.temp %>% inner_join(long,"CD_GEOCODI")

# create a distance matrix
m <- distm(setor.map[2:3], setor.map[2:3], fun = distVincentyEllipsoid)

# replace the diagonal with NA
diag(m) <- 0

# make column names for the distance matrix
colnames(m) <- paste0('r',1:nrow(setor.map))

#Standadized the rows
for(i in 1:nrow(m)){
  m[i,]<-m[i,]/sum(m[i,])
}

#Create the weighted Amount
x<-as.matrix(setor.map[,c(-1,-2,-3)])
x[is.na(x)] <- 0
x<-apply(x,2,function(x) (x-min(x))/(max(x)-min(x)))
x<-m%*%x
setor.map[,c(-1,-2,-3)]<-apply(setor.map[,c(-1,-2,-3)],2,function(x) (x-min(x))/(max(x)-min(x)))
x<-data.frame(setor.map[,c(1,2,3)],x)

#Replace NA 
setor.map[is.na(setor.map)]<-x[is.na(setor.map)]

#Colnames matrix
x<-x[,c(-1,-2,-3)]
colnames(x) <- paste0('neigh',colnames(x))

#Final Data
final<-cbind(setor.map,x)

#Rename weight matrix
weight.Mat<-m

rm(list = ls()[!ls() %in% c("final", "map","real.df","weight.Mat","setor.map")])

save.image("Data\\FinalData.RData")
