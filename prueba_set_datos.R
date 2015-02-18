setwd("C:\\Users\\Usuarioç\\Desktop\\carlos\\Tesis")
getwd()
rm(list = ls())
library(lubridate)

# Content of Files
# users.dat: consists of a set of users such that each user has a unique id and a geospatial location (latitude and longitude) that represents the user home town location.
# venues.dat: consists of a set of venues (e.g., restaurants) such that each venue has a unique id and a geospatial location (lattude and longitude).
# checkins.dat: marks the checkins (visits) of users at venues. Each check-in has a unique id as well as the user id and the venue id.
# socialgraph.dat: contains the social graph edges (connections) that exist between users. Each social connection consits of two users (friends) represented by two unique ids (first_user_id and second_user_id).
# ratings.dat: consists of implicit ratings that quantifies how much a user likes a specific venue.

usersData<-read.csv("datasets_csv/users.csv")
venuesData<-read.csv("datasets_csv/venues.csv")
checkinsData<-read.csv("datasets_csv/checkins.csv")
socialgraphData<-read.csv("datasets_csv/socialgraph.csv")
ratingsData<-read.csv("datasets_csv/ratings.csv")


socialgraphData$tiempo<-0
write.table(socialgraphData,file="algoritmo/datos/grafo.txt", row.names = F,col.names=F)
head(checkinsData,n=100)
str(checkinsData)
checkinsData$created_at<-as.character(checkinsData$created_at)
as.POSIXct("2013-07-24 23:55:26")
checkinsData$tiempo<-as.POSIXct(checkinsData$created_at)
checkinsData$tiempoNum<-as.numeric(checkinsData$tiempo)
checkinsData <- checkinsData[order(checkinsData$tiempo),] 
write.table(checkinsData[,c("user_id","venue_id","tiempoNum")],file="algoritmo/datos/actividades.txt", row.names = F,col.names=F)

write.table(venuesData[,1],file="algoritmo/datos/trainActividades.txt", row.names = F,col.names=F)

