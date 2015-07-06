library(plyr)
library(ggplot2)
library(reshape2)
library(lubridate)
### Analisis de los rating de NY.
ratings<-read.csv(file = "datasets/foursquare/datasets_csv/rating_NY_UE.csv",header=F);
### Analisis de los ratings reducidos.
ratings<-read.csv(file = "datasets/foursquare/datasets_csv/rating_NY_UE.csv",header=F);
head(ratings)
names(ratings)<-c("user","item","rating")
## Cantidad de usuario y de items.
str(ratings)
length(unique(ratings$user))
length(unique(ratings$item))
mResumen<- data.frame(media=mean(ratings$rating),
                      max=max(ratings$rating), 
                      min=min(ratings$rating),
                      var=var(ratings$rating),
                      sd=sd(ratings$rating))
mResumen
ggplot(ratings,aes(x=factor(0),y=rating) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

png(filename="informe-jaiio-2015/figuras/ny_distribucion_rating.png",width = 200 , height=200)
ggplot(ratings,aes(x=rating))+geom_histogram(binwidth=.4)
dev.off()

1-(264461 /(96467*88578))
### cuantos lugares visitó cada usuario
visitas<-ddply(ratings,.(user), summarise, visitas=length(item))
visitas<-visitas[order(visitas$visitas,decreasing = T),]
#los 5 primeros usuarios con mas visitas.
head(visitas)
tail(visitas)
vResumen<- data.frame(media=mean(visitas$visitas),
                      max=max(visitas$visitas), 
                      min=min(visitas$visitas),
                      var=var(visitas$visitas),
                      sd=sd(visitas$visitas))

vResumen
## Distribucion de las visitas realizadas por los usuarios.
usuarios.boxplot<-ggplot(visitas,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(visitas, aes(x=visitas)) + geom_density() +scale_x_continuous(breaks = seq(20, 500, by = 100)) 

ggplot(visitas, aes(x=visitas)) + geom_histogram() +scale_x_continuous(breaks = seq(20, 500, by = 100)) 
twogrp<-c(rnorm(10)+4,rnorm(10)+20)
gap.barplot(visitas$visitas, gap=c(8,18),horiz=F)

plot(3:10, main = "Axis break test")
# put a break at the default axis and position
axis.break()
axis.break(2, 2.9, style = "zigzag")
twogrp <- c(rnorm(10) + 4, rnorm(10) + 20)
gap.plot(twogrp,gap = c(8,16), xlab = "Index", ylab = "Group values",
         main = "Two separated groups with gap axis break",
         col = c(rep(2, 10), rep(3, 10)), ytics = c(3, 5, 18, 20))
legend(12, 6, c("Low group", "High group"), pch = 1, col = 2:3)



### los lugares cuantas veces fueron visitados.
lugares<-ddply(ratings,.(item), summarise, visitas=length(user), 
               totalRating=sum(rating))
lugares<-lugares[order(lugares$visitas,decreasing = T),]
head(lugares)
tail(lugares)
lResumen<- data.frame(media=mean(lugares$visitas),
                      max=max(lugares$visitas), 
                      min=min(lugares$visitas),
                      var=var(lugares$visitas),
                      sd=sd(lugares$visitas))

lResumen
##Distribucion de las cantidades de veces que fueron visitados los lugares.
lugares.boxplot<-ggplot(lugares,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(lugares, aes(x=visitas)) + geom_density() +scale_x_continuous(breaks = seq(1, 500, by = 100)) 

png(filename="informe-jaiio-2015/figuras/ny_distribucion_visitas.png",width = 400 , height=400)
multiplot(usuarios.boxplot,lugares.boxplot, cols=2)
dev.off()

## Frecuencia de las cantidad de visitas.
visitCant<-as.data.frame(table(lugares$visitas), stringsAsFactors=TRUE)
names(visitCant)<-c("CantVisitas","Freq")
head(visitCant)
ggplot(visitCant[50:239,], aes(x=(CantVisitas),y=Freq)) + geom_histogram(stat="identity") 

## Solapamiento
solapamiento<-read.csv("datasets/foursquare/datasets_csv/solap_NY_Freq.csv")
solaPorcentaje<-read.csv("datasets/foursquare/datasets_csv/solap_NY_Porcentaje_Freq.csv")
ggplot(solapamiento, aes(x=as.factor(overlap),y=frecuencia)) + geom_histogram(stat="identity") 
str(solapamiento)
sum(solapamiento$frecuencia)
str(solaPorcentaje)
sum(solaPorcentaje$frecuencia)
(((96467*96467)/2)-96467)-sum(solaPorcentaje$frecuencia)
4652941045-96467

m <- combn(96467, 2)
dim(m)[2]
c(2, choose(96467, 2))
c(2, choose(4, 2))
229799800+127323543
###Analisis Cantidad de Vecinos.
### vecinos calculados con la correlacion Pearson.

usuariosVecinos<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinos.csv",header=T)
head(usuariosVecinos)
tail(usuariosVecinos)
vecinosP<-ddply(usuariosVecinos,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0) )

### vecinos calculados con la distancia Euclidea.
usuariosVecinosE<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosE.csv",header=T)
head(usuariosVecinosE)
tail(usuariosVecinosE)
ddply(usuariosVecinosE,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0) )

### vecinos calculados con la similitud coseno.
usuariosVecinosC<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosC.csv",header=T)
head(usuariosVecinosC)
tail(usuariosVecinosC)
vecinosC<-ddply(usuariosVecinosC,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0))

### vecinos calculados con la similitud tanimoto.
usuariosVecinosT<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosT.csv",header=T)
head(usuariosVecinosT)
tail(usuariosVecinosT)
ddply(usuariosVecinosT,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0))

##################################
##########Analizando checkins en UE#####
getwd()
library(lubridate)
library(ggmap)
checkinsUE<-read.csv(file="datasets/foursquare/datasets_csv/checkinsVUE.csv")
head(checkinsUE)

#### mapa de checkins en united state
png(filename="mapa_check_ue.png",width = 1600 , height=900)
map <- get_map(location = "united states", zoom = 4)
ggmap(map, fullpage = TRUE) + 
    geom_point(aes(x = lon_venue, y = lat_venue), colour = "red",alpha = 0.1, size = 4, data = checkinsUE)
dev.off()

png(filename="mapa_check_densidad_ue.png",width = 1600 , height=900)
map <- get_map(location = "united states", zoom = 4, color = "bw")
euMap <- ggmap(map, base_layer = ggplot(aes(x = lon_venue, y = lat_venue),
                                                 data = checkinsUE))
euMap +
    stat_density2d(aes(x = lon_venue, y = lat_venue, fill = ..level.., alpha = ..level..),
                   bins = 5, geom = "polygon",
                   pseudocount=0.0001,
                   data = checkinsUE) +
    scale_fill_gradient(low = "black", high = "red") 
dev.off()

#### california
sort(table(checkinsUE$city_venue))

checkinsCalifornia<-checkinsUE[checkinsUE$city_venue=="california",]
checkinsCalifornia$dataTime<-parse_date_time(checkinsCalifornia$created_at,"%y%m%d %H%M%S",tz="America/Los_Angeles")
checkinsCalifornia$wday<-wday(checkinsCalifornia$dataTime,label=T)
checkinsCalifornia$hour<-hour(checkinsCalifornia$dataTime)
checkinsCalifornia$partDay<-""
checkinsCalifornia$partDay[6<=checkinsCalifornia$hour & checkinsCalifornia$hour<=12]<-"morning"
checkinsCalifornia$partDay[13<=checkinsCalifornia$hour & checkinsCalifornia$hour<=19]<-"afternoon"
checkinsCalifornia$partDay[20<=checkinsCalifornia$hour & checkinsCalifornia$hour<=23]<-"evening"
checkinsCalifornia$partDay[0<=checkinsCalifornia$hour & checkinsCalifornia$hour<=5]<-"night"

#### new york
checkinsNY<-checkinsUE[checkinsUE$city_venue=="new york",]
checkinsNY$dataTime<-parse_date_time(checkinsNY$created_at,"%y%m%d %H%M%S",tz="America/New_York")
checkinsNY$wday<-wday(checkinsNY$dataTime,label=T)
checkinsNY$hour<-hour(checkinsNY$dataTime)
checkinsNY$partDay<-""
checkinsNY$partDay[6<=checkinsNY$hour & checkinsNY$hour<=12]<-"morning"
checkinsNY$partDay[13<=checkinsNY$hour & checkinsNY$hour<=19]<-"afternoon"
checkinsNY$partDay[20<=checkinsNY$hour & checkinsNY$hour<=23]<-"evening"
checkinsNY$partDay[0<=checkinsNY$hour & checkinsNY$hour<=5]<-"night"

png(filename="informe-jaiio-2015/figuras/mapa_check_densidad_ny.png",width = 200 , height=200)
ggplot(checkinsNY,aes(x=lon_venue,y = lat_venue))+stat_binhex(bins=50)+
    scale_fill_gradientn(colours=c("white","blue"),name = "Frequency",na.value=NA)+
    facet_wrap(~ partDay)
dev.off()
#,source = "osm"
png(filename="informe-jaiio-2015/figuras/mapa_check_densidad_wday2_ny.png",width = 1600 , height=900)
houston <- get_map(location = "new york", zoom = 14, color = "bw")
HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = lon_venue, y = lat_venue),data = checkinsNY))
HoustonMap +
    stat_density2d(aes(x = lon_venue, y = lat_venue, fill = ..level.., alpha = ..level..),
                   bins = 5, geom = "polygon", data = checkinsNY) +
    scale_fill_gradient(low = "black", high = "red") +
    facet_wrap(~ wday)
dev.off()

png(filename="informe-jaiio-2015/figuras/mapa_check_densidad_ny.png",width = 400 , height=400)
SHA_MAP_zoom12 <- ggmap(get_map(maptype = "terrain", location = "new york", 
                                zoom = 14, source = "google"), base_layer = ggplot(aes(x = lon_venue, y = lat_venue), 
                                                                                   data = checkinsNY), extent = "panel")

SHA_MAP_zoom12 + stat_density2d(aes(x = lon_venue, y = lat_venue, fill = ..level..,alpha = ..level..), size = 2, bins = 5, data = checkinsNY, geom = "polygon") + 
    scale_alpha(range = c(0.4, 0.75), guide = FALSE) +
    scale_fill_gradient(low = "black", high = "red") 
#+ facet_wrap( ~wday)
dev.off()


bb <- attr(HoustonMap, "bb")

sum(checkinsNY$lat_venue < bb$ll.lat | 
        checkinsNY$lat_venue > bb$ur.lat | 
        checkinsNY$lon_venue < bb$ll.lon | 
        checkinsNY$lon_venue > bb$ur.lon)


head(checkinsNY)
##### whather
##KLAX
##KCQT
library(weatherData)
checkinsNY$dataTime[1:2]
year(checkinsNY$dataTime[1:2])
month(checkinsNY$dataTime[1:2])
table(checkinsNY$dataTime)
checkinsNY$StrDate<-paste(year(checkinsNY$dataTime),month(checkinsNY$dataTime),day(checkinsNY$dataTime),sep="-")
head(checkinsNY)
length(unique(checkinsNY$StrDate))
max(checkinsNY$dataTime)
min(checkinsNY$dataTime)
max(checkinsNY$dataTime)-min(checkinsNY$dataTime)
int1 <- new_interval(min(checkinsNY$dataTime),max(checkinsNY$dataTime))
int_length(int1)
period(int1,"day")
int_diff(max(checkinsNY$dataTime),min(checkinsNY$dataTime))

dates <- now() + days(1:10)
int_diff(dates)
day(checkinsNY$dataTime)
for(i in 1:nrow(checkinsNY)){
    dat <- getWeatherForDate("KLAX", ex, ex,opt_all_columns = TRUE)    
}
    
distCosine(c(0,0),c(90,90))
xy <- rbind(c(0,0),c(90,90),c(10,10),c(-120,-45))
distm(xy)
xy2 <- rbind(c(0,0),c(10,-10))


data(USAirportWeatherStations)
head(USAirportWeatherStations$State)
USAirportWeatherStations[USAirportWeatherStations$State=="NY",]

*****
head(checkinsNY)    
head(ratings)
merge.check<-merge(checkinsNY, )


