library("recommenderlab")
library("igraph")
### Reduccion de los rating a usuarios con cantidad de rating >20
ratingsMean<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv")
rRMatrix <- as(ratingsMean, "realRatingMatrix")
rRMatrixReducido<- rRMatrix[rowCounts(rRMatrix)>20,]
ratingsMeanReducido<-(as(rRMatrixReducido, "data.frame"))
write.table(ratingsMeanReducido,file="datasets/foursquare/datasets_csv/ratingsMeanReducido.csv", row.names = F, col.names= F,sep=",",quote = F)

### Reduccion de la red social a usuarios con cantidad de rating >20.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv")
ratingsMeanReducido<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMeanReducido.csv",header=F)
vertices<-unique(ratingsMeanReducido$V1)
vertices<-as.character(vertices)
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)
vertex<-vertices[which (is.element(vertices, socialgraphData$first_user_id))]
grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoReducido<-induced.subgraph(grafo, vids=vertex)
grafoReducidoDF<-get.data.frame(grafoReducido, what=c( "edges"))
write.table(grafoReducidoDF,file=getDataSetPath("redSocialReducida.csv"), 
            row.names = F, col.names= F,sep=",",quote = F)

### Agregar ciudad a venues y users(solo UE)
library(sp)
library(maps)
library(maptools)
library(rgdal)
library(geonames)
venues<-read.csv(file="datasets/foursquare/datasets_csv/venues.csv")
str(venues)
venuesSinNA<-venues[complete.cases(venues),]
venuesSinNA<-venuesSinNA[venuesSinNA$latitude!= 91.4313194,]
venuesCity<-latlong2state(venuesSinNA[1:2,c("longitude","latitude")])
venuesSinNA<-cbind(venuesSinNA,data.frame(city=venuesCity))
venuesSinNA<-venuesSinNA[complete.cases(venuesSinNA),]

# options(geonamesUsername="carlosrios") 
# options(geonamesHost="api.geonames.org")
# source(system.file("tests","testing.R",package="geonames"),echo=TRUE)
# results<-GNwikipediaSearch("oriole", maxRows = 10)
# GNfindNearbyStreets(45.54058, -73.59652)
# GNfindNearestAddress(45.54058, -73.59652)
# GNfindNearestIntersection("45.54058", "-73.59652")
# GNcities( 45.54058, east, -73.59652, west, lang = "en", maxRows = 10)
# GNfindNearbyPlaceName(lat = 45.54058,lng =-73.59652 )
# testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
# latlong2state(testPoints)

write.csv(venuesSinNA,file="datasets/foursquare/datasets_csv/venuesUE.csv", row.names = F)

users<-read.csv(file="datasets/foursquare/datasets_csv/users.csv")
usersSinNA<-users[complete.cases(users),]
usersCity<-latlong2state(usersSinNA[,c("longitude","latitude")])
usersSinNA<-cbind(usersSinNA,data.frame(city=usersCity))
usersSinNA<-usersSinNA[complete.cases(usersSinNA),]
write.csv(usersSinNA,file="datasets/foursquare/datasets_csv/usersUE.csv", row.names = F)

### Reducir rating solo users y venues de UE.
rating<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv",header = F)
names(rating)<-c("user_id","venue_id","rating")
usersUE<-read.csv(file="datasets/foursquare/datasets_csv/usersUE.csv")
venuesUE<-read.csv(file="datasets/foursquare/datasets_csv/venuesUE.csv")
head(venuesUE)
head(usersUE)
ratingUE<-merge(rating, usersUE,by.x="user_id",by.y="id",all.x=T)
ratingUE<-merge(ratingUE, venuesUE,by.x="venue_id",by.y="id",all.x=T)
head(ratingUE)
names(ratingUE)<-c("venue_id","user_id","rating","lat_user","lon_user","city_user","lat_venue","lon_venue","city_venue")
ratingUE<-ratingUE[complete.cases(ratingUE),]
write.csv(ratingUE,file="datasets/foursquare/datasets_csv/rating_city_UE.csv", row.names = F)
write.csv(ratingUE[,1:3],file="datasets/foursquare/datasets_csv/ratingUE.csv", row.names = F)

### Reducir grafo solo users de UE.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv")
usersUE<-read.csv(file="datasets/foursquare/datasets_csv/usersUE.csv")

vertices<-unique(usersUE$id)
vertices<-as.character(vertices)
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)
vertex<-vertices[which(is.element(vertices, socialgraphData$first_user_id))]
grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoUE<-induced.subgraph(grafo, vids=vertex)
grafoUEDF<-get.data.frame(grafoUE, what=c( "edges"))
write.table(grafoUEDF,file="datasets/foursquare/datasets_csv/grafoUE.csv",row.names = F, col.names= F,sep=",",quote = F)
grafoUEund<- as.undirected(grafoUE,mode ="collapse")
write.graph(grafoUEund,file="datasets/foursquare/datasets_csv/grafoUE.graphml",format="graphml")

### Reducir de checkins a usuers y venues de UE.
checkins<-read.csv("datasets/foursquare/datasets_csv/checkins.csv")
usersUE<-read.csv(file="datasets/foursquare/datasets_csv/usersUE.csv")
venuesUE<-read.csv(file="datasets/foursquare/datasets_csv/venuesUE.csv")
checkinsUE<-merge(checkins,usersUE,by.x="user_id",by.y="id",all.x=T)
names(checkinsUE)<-c("user_id","id","venue_id","latitude","longitude","created_at","lat_user","lon_user","city_user")
checkinsUE<-merge(checkinsUE,venuesUE,by.x="venue_id",by.y="id",all.x=T)
names(checkinsUE)<-c("venue_id","user_id","id","latitude","longitude","created_at","lat_user","lon_user","city_user","lat_venue","lon_venue","city_venue")
checkinsUE$latitude<-NULL
checkinsUE$longitude<-NULL
checkinsUE$id<-NULL
checkinsUE<-checkinsUE[complete.cases(checkinsUE),]
write.csv(checkinsUE,file="datasets/foursquare/datasets_csv/checkinsUE.csv", row.names = F)


### Reducir rating solo a venues de UE.
rating<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv",header = F)
names(rating)<-c("user_id","venue_id","rating")
venuesUE<-read.csv(file="datasets/foursquare/datasets_csv/venuesUE.csv")
ratingVUE<-merge(rating, venuesUE,by.x="venue_id",by.y="id",all.x=T)
head(ratingVUE)
names(ratingVUE)<-c("venue_id","user_id","rating","lat_venue","lon_venue","city_venue")
ratingVUE<-ratingVUE[complete.cases(ratingVUE),]
write.csv(ratingVUE,file="datasets/foursquare/datasets_csv/rating_city_VUE.csv", row.names = F)
## guardo sin nombre de columnas para mahout
write.table(ratingVUE[,c("user_id","venue_id","rating")],file="datasets/foursquare/datasets_csv/ratingVUE.csv", row.names = F, col.names= F,sep=",",quote = F)


### Reducir grafo solo users que hicieron check en UE.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv")
ratingVUE<-read.csv(file="datasets/foursquare/datasets_csv/ratingVUE.csv",header=F)
vertices<-unique(ratingVUE$V1)
vertices<-as.character(vertices)
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)
vertex<-vertices[which(is.element(vertices, socialgraphData$first_user_id))]
grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoUE<-induced.subgraph(grafo, vids=vertex)
grafoUEDF<-get.data.frame(grafoUE, what=c( "edges"))
write.table(grafoUEDF,file="datasets/foursquare/datasets_csv/grafo_users_check_UE.csv",row.names = F, col.names= T,sep=",",quote = F)
grafoUEund<- as.undirected(grafoUE,mode ="collapse")
write.graph(grafoUEund,file="datasets/foursquare/datasets_csv/grafo_users_check_UE.graphml",format="graphml")


### Reducir de checkins en venues de UE.
checkins<-read.csv("datasets/foursquare/datasets_csv/checkins.csv")
venuesUE<-read.csv(file="datasets/foursquare/datasets_csv/venuesUE.csv")
checkinsUE<-merge(checkins,venuesUE,by.x="venue_id",by.y="id",all.x=T)
head(checkinsUE)
names(checkinsUE)<-c("venue_id","id","user_id","latitude","longitude","created_at","lat_venue","lon_venue","city_venue")
checkinsUE$latitude<-NULL
checkinsUE$longitude<-NULL
checkinsUE$id<-NULL
checkinsUE<-checkinsUE[complete.cases(checkinsUE),]
write.csv(checkinsUE,file="datasets/foursquare/datasets_csv/checkinsVUE.csv", row.names = F)

### Reducir rating solo a venues de NY- levantar solo los data set de UE.
rating<-read.csv(file="datasets/foursquare/datasets_csv/rating_city_VUE.csv",header = T)
ratingNY<-rating[rating$city_venue=="new york",]
## guardo sin nombre de columnas para mahout
write.table(ratingNY[,c("user_id","venue_id","rating")],file="datasets/foursquare/datasets_csv/rating_NY_UE.csv", row.names = F, col.names= F,sep=",",quote = F)



### Reducir grafo solo users que hicieron check en NY.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv")
ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/rating_NY_UE.csv",header=F)
vertices<-unique(ratingVNY$V1)
vertices<-as.character(vertices)
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)
vertex<-vertices[which(is.element(vertices, socialgraphData$first_user_id))]
grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoUE<-induced.subgraph(grafo, vids=vertex)
grafoUEDF<-get.data.frame(grafoUE, what=c( "edges"))
write.table(grafoUEDF,file="datasets/foursquare/datasets_csv/grafo_users_check_NY_UE.csv",row.names = F, col.names= T,sep=",",quote = F)
grafoUEund<- as.undirected(grafoUE,mode ="collapse")
write.graph(grafoUEund,file="datasets/foursquare/datasets_csv/grafo_users_check_NY_UE.graphml",format="graphml")

### Reducir grafo solo users que hicieron check en NY. usuarios sin datos perdidos
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv")
ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/NY/rating_NY2_UE.csv",header=F)
vertices<-unique(ratingVNY$V1)
vertices<-as.character(vertices)
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)
vertex<-vertices[which(is.element(vertices, socialgraphData$first_user_id))]
vertex2<-vertices[which(is.element(vertices, socialgraphData$second_user_id))]

grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoUE<-induced.subgraph(grafo, vids=vertex)
grafoUEDF<-get.data.frame(grafoUE, what=c( "edges"))
write.table(grafoUEDF,file="datasets/foursquare/datasets_csv/NY/grafo_users_check_NY2_UE.csv",row.names = F, col.names= T,sep=",",quote = F)
grafoUEund<- as.undirected(grafoUE,mode ="collapse")
write.graph(grafoUEund,file="datasets/foursquare/datasets_csv/NY/grafo_users_check_NY2_UE.graphml",format="graphml")



### crear data set con user and venue NY
ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/rating_NY_UE.csv",header=F)
venues<-read.csv(file="datasets/foursquare/datasets_csv/venues.csv",colClass="character")
users<-read.csv(file="datasets/foursquare/datasets_csv/users.csv",colClass="character")
users$id<-as.numeric(users$id)
venues$id<-as.numeric(venues$id)
m1<- merge(ratingVNY, users, by.x = "V1", by.y = "id")
names(m1)<-c("User","Item","Rating","latitudeUser","longitudeUser")
m1<- merge(m1, venues, by.x = "Item", by.y = "id")
names(m1)<-c("Item","User","Rating","latitudeUser","longitudeUser","latitudeItem","longitudeItem")
m1<-m1[complete.cases(m1),]
usersNY<-m1[!duplicated(m1$User),c("User","latitudeUser","longitudeUser")]
venueNY<-m1[!duplicated(m1$Item),c("Item","latitudeItem","longitudeItem")]
head(usersNY)
head(venueNY)
write.table(m1[,c("User","Item","Rating")],file="datasets/foursquare/datasets_csv/rating_NY2_UE.csv",row.names = F, col.names= F,sep=",",quote = F)
write.table(usersNY,file="datasets/foursquare/datasets_csv/users_check_NY2_UE.csv",row.names = F, col.names= T,sep=",",quote = F)
write.table(venueNY,file="datasets/foursquare/datasets_csv/venues_check_NY2_UE.csv",row.names = F, col.names= T,sep=",",quote = F)

######## Reduccion de checkins  por NY #####
checkinsVUE<-read.csv(file="datasets/foursquare/datasets_csv/checkinsVUE.csv",header=T)
head(checkinsVUE)
str(checkinsVUE)
checkinsVUE$city_venue<-as.character(checkinsVUE$city_venue)
table(checkinsVUE$city_venue)
checkinsVUENY<-checkinsVUE[checkinsVUE$city_venue=="new york",]
head(checkinsVUENY)
table(checkinsVUENY$city_venue)

####### Agrego variables derivadas de la fecha
library(lubridate)
checkinsVUENY$dataTime<-parse_date_time(checkinsVUENY$created_at,"%y%m%d %H%M%S",tz="America/New_York")
checkinsVUENY$wday<-wday(checkinsVUENY$dataTime,label=T)
checkinsVUENY$hour<-hour(checkinsVUENY$dataTime)
checkinsVUENY$partDay<-""
checkinsVUENY$partDay[6<=checkinsVUENY$hour & checkinsVUENY$hour<=12]<-"morning"
checkinsVUENY$partDay[13<=checkinsVUENY$hour & checkinsVUENY$hour<=19]<-"afternoon"
checkinsVUENY$partDay[20<=checkinsVUENY$hour & checkinsVUENY$hour<=23]<-"evening"
checkinsVUENY$partDay[0<=checkinsVUENY$hour & checkinsVUENY$hour<=5]<-"night"
checkinsVUENY$isWday<-FALSE
checkinsVUENY[checkinsVUENY$wday=="Sun",]$isWday<-TRUE
checkinsVUENY[checkinsVUENY$wday=="Sat",]$isWday<-TRUE

str(checkinsVUENY)
head(checkinsVUENY)
table(checkinsVUENY$wday)
table(checkinsVUENY$isWday)
table(checkinsVUENY$partDay)

#######  Reduccion de datos a Checkins en NY y personas que realizaron 3 o mas
library(plyr)
library(ggplot2)
ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/NY/rating_NY2_UE.csv",header=F)
venues<-read.csv(file="datasets/foursquare/datasets_csv/NY/venues_check_NY2_UE.csv",colClass="character")
users<-read.csv(file="datasets/foursquare/datasets_csv/NY/users_check_NY2_UE.csv",colClass="character")
checkinsVUE<-read.csv(file="datasets/foursquare/datasets_csv/NY/checkins_check_NY_UE.csv",header=T)

head(ratingVNY)
rating_cantidad<-ddply(ratingVNY,.(V1),summarize, freq=length(V1))
head(rating_cantidad)
ratingVNY[ratingVNY$V1==38,]
table(rating_cantidad$freq)
rating_cantidad<-rating_cantidad[rating_cantidad$freq>1,]
hist(rating_cantidad$freq)
boxplot(rating_cantidad$freq)
length(unique(ratingVNY$V1))
str(rating_cantidad)
str(ratingVNY)
merge<-merge(rating_cantidad,ratingVNY,by.x = "V1",by.y = "V1")
length(unique(merge$V1))
head(merge)
ratingVNY[ratingVNY$V1==38,]
merge[merge$V1==38,]
names(merge)<-c("user_id","freq","venue_id","rating")
head(merge)
write.table(merge[,c("user_id","venue_id","rating")],file="datasets/foursquare/datasets_csv/NY/rating_NY_2_mas_check_UE.csv", row.names = F, col.names= F,sep=",",quote = F)
head(users)
mergeUser<-merge(rating_cantidad,users,by.x="V1",by.y="User")
head(mergeUser)
names(mergeUser)<-c("User","freq","latitudeUser","longitudeUser")
write.table(mergeUser[,c("User","latitudeUser","longitudeUser")],file="datasets/foursquare/datasets_csv/NY/users_check_NY_2_mas_check_UE.csv", row.names = F, col.names= T,sep=",",quote = F)

rating_cantidad_venues<-ddply(merge,.(venue_id),summarize, freq=length(venue_id))
head(venues)
head(rating_cantidad_venues)
length(unique(merge$venue_id))
mergeVenues<-merge(rating_cantidad_venues,venues,by.x="venue_id",by.y="Item")
head(mergeVenues)
names(mergeVenues)<-c("Item","freq","latitudeItem","longitudeItem")
write.table(mergeVenues[,c("Item","latitudeItem","longitudeItem")],file="datasets/foursquare/datasets_csv/NY/venues_check_NY_2_mas_check_UE.csv", row.names = F, col.names= T,sep=",",quote = F)


ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/NY/rating_NY_2_mas_check_UE.csv",header=F)
head(ratingVNY,50)
rating_cantidad<-ddply(ratingVNY,.(V1),summarize, freq=length(V1))
head(rating_cantidad)
table(rating_cantidad$freq)
ratingVNY$V4[ratingVNY$V3>=3]<-1
table(ratingVNY$V4)
ratingVNY[ratingVNY$V1==29489,]
305774
rating_cantidad_cero<-ddply(ratingVNY,.(V1),summarize, freq=length(V4==0))
head(rating_cantidad_cero)

solap<-read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/distanciasJaccardLiked.csv",sep = "\t",header=F)
head(solap)
hist(solap$V3)
summary(solap$V3)
boxplot(solap$V3)
which.max(solap$V3)
dim(combn(40758,2))
str(solap)
which.min(solap$V3)
length(unique(solap$V2))
solap[42336,]
0.001<0.00237

solapLH<-read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/distanciasJaccardLikedAndHated.csv",sep = "\t",header=F)
head(solapLH)
str(solapLH)
summary(solapLH$V3)
hist(solapLH$V3)
boxplot(solapLH$V3)
length(unique(solapLH$V2))

ratingVNY[] 776162
which(ratingVNY$V1==1111844)

cantidadSolap<-read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/cantidadSolpamientoPorUsuario.csv",sep = "\t",header=F)
str(cantidadSolap)
boxplot(cantidadSolap$V2)
hist(cantidadSolap$V2)
table(cantidadSolap$V2)
head(cantidadSolap)

cantidadSolapL<-read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/cantidadSolpamientoPorUsuarioLiked.csv",sep = "\t",header=F)
head(cantidadSolapL)
str(cantidadSolapL)
table(cantidadSolapL$V2)
40758-19701

((40758*40758)-40758)/2

((21057*21057)-21057)/2

##### whather  ################
library(weatherData)
USAirportWeatherStations[USAirportWeatherStations$State=="NY",]
checkinsVUENY$StrDate<-paste(year(checkinsVUENY$dataTime),month(checkinsVUENY$dataTime),day(checkinsVUENY$dataTime),sep="-")

for(i in 1:nrow(checkinsNY)){
    dat <- getWeatherForDate("KDSV", ex,opt_all_columns = TRUE)    
}

fechas<-data.frame(fecha= checkinsVUENY[!duplicated(checkinsVUENY$StrDate),c("StrDate")])

dat1 <- getWeatherForDate(station_id = "KDSV", start_date = c("2011-12-12","2011-12-9"),opt_all_columns = TRUE)  
dat2 <- getWeatherForDate("KDSV", "2011-12-12",opt_all_columns = TRUE)  
f<-sapply(fechas,getWeatherForDate,station_id = "KDSV",opt_all_columns = TRUE)



length(unique(checkinsVUENY$StrDate)) 
table(checkinsVUENY$StrDate)

class()

write.table(checkinsVUENY,file="datasets/foursquare/datasets_csv/NY/checkins_check_NY_UE.csv",row.names = F, col.names= T,sep=",",quote = F)
str(checkinsVUENY)




