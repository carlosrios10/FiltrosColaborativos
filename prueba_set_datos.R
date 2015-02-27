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

#### reducir red social

socialgraphData<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/socialgraph.csv")
ratingsMeanReducido<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/ratingsMeanReducido7.csv",
                              header=F)
table(ratingsMeanReducido$V1)
vertices<-data.frame(ver=vertices)
pp<-socialgraphData[1:20,]
length(unique(pp$first_user_id))
length(unique(pp$second_user_id))

redSocialReducida<-merge(socialgraphData,vertices, by.x="first_user_id", by.y="ver",all.y=T)
redSocialReducida2<-merge(redSocialReducida,vertices, by.x="second_user_id", by.y="ver",all.y=T)

17778*2
head(redSocialReducida)
tail(redSocialReducida)
length(unique(redSocialReducida2$first_user_id))
length(unique(redSocialReducida2$second_user_id))
write.csv(redSocialReducida,file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/redSocialReducida7.csv", 
            row.names = F)

### 
df<- read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/dataset2.csv")
dfMatrix <- as(df, "realRatingMatrix")
dfm<-as(dfMatrix, "matrix")
tdfm<-t(dfm)
cor(tdfm)
dist(dfm)
