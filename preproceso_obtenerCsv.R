wd<-switch(Sys.info()["nodename"][[1]],
                           'USUARIOÇ-PC'="C:/Users/Usuarioç/Desktop/carlos/Tesis/workspaceR")
wd<-getwd()
setwd(paste(wd,"workspaceR/Recomendacion/FiltrosColaborativos",sep = "/"))
# This data set contains 2153471 users, 1143092 venues, 1021970 check-ins, 27098490 social connections, and 2809581 ratings that users assigned to venues;
# Content of Files
# users.dat: consists of a set of users such that each user has a unique id and a geospatial location (latitude and longitude) that represents the user home town location.
# venues.dat: consists of a set of venues (e.g., restaurants) such that each venue has a unique id and a geospatial location (lattude and longitude).
# checkins.dat: marks the checkins (visits) of users at venues. Each check-in has a unique id as well as the user id and the venue id.
# socialgraph.dat: contains the social graph edges (connections) that exist between users. Each social connection consits of two users (friends) represented by two unique ids (first_user_id and second_user_id).
# ratings.dat: consists of implicit ratings that quantifies how much a user likes a specific venue.
library(data.table)
readLines("datasets/foursquare/umn_foursquare_datasets/venues.dat",n=2)
venuesData <- read.delim('datasets/foursquare/umn_foursquare_datasets/venues.dat', header = F, sep="|",skip=2) 
names(venuesData)<-c("id","latitude","longitude")
head(venuesData)
tail(venuesData)
dim(venuesData)
venuesData<-venuesData[-1143091,]
write.csv(venuesData,file="datasets_csv/venues.csv", row.names = F)

readLines("datasets/foursquare/umn_foursquare_datasets/socialgraph.dat",n=2)
socialData<-read.delim("datasets/foursquare/umn_foursquare_datasets/socialgraph.dat", header = F, sep="|",skip=2) 
names(socialData)<-c("first_user_id","second_user_id")
head(socialData)
tail(socialData,n=100)
socialData<-socialData[-27098473:-27098488,]
socialData$first_user_id<-format(socialData$first_user_id,scientific = FALSE,trim=T)
socialData$second_user_id<-format(socialData$second_user_id,scientific = FALSE,trim=T)
str(socialData)
write.csv(socialData,file="datasets/foursquare/datasets_csv/socialgraph.csv", row.names = F,quote = F)

readLines("umn_foursquare_datasets/users.dat",n=2)
usersData<-read.delim('umn_foursquare_datasets/users.dat', header = F, sep="|",skip=2) 
names(usersData)<-c("id", "latitude", "longitude")
head(usersData)
tail(usersData)
usersData<-usersData[-2153470,]
write.csv(usersData,file="datasets_csv/users.csv", row.names = F)

readLines("umn_foursquare_datasets/checkins.dat",n=2)
checkinsData<-read.delim('umn_foursquare_datasets/checkins.dat', header = F, sep="|",skip=2) 
names(checkinsData)<-c("id","user_id","venue_id","latitude","longitude","created_at")
head(checkinsData)
tail(checkinsData)
checkinsData<-checkinsData[-1021967,]
write.csv(checkinsData,file="datasets_csv/checkins.csv", row.names = F)

readLines("foursquare/umn_foursquare_datasets/ratings.dat",n=2)
ratingsData<-read.delim('foursquare/umn_foursquare_datasets/ratings.dat', header = F, sep="|",skip=2) 
names(ratingsData)<-c("user_id","venue_id","rating")
head(ratingsData)
tail(ratingsData)
str(ratingsData)
ratingsData<-ratingsData[-2809581,]
ratingsData$user_id<-as.numeric(as.character(ratingsData$user_id))
write.csv(ratingsData,file="foursquare/datasets_csv/ratings.csv", row.names = F,quote = F)

ratingsData<-fread("foursquare/datasets_csv/ratings.csv",colClasses = c("numeric","numeric","numeric"))
ratingsMean<-ratingsData[,mean(rating),by=list(user_id,venue_id)]
setnames(ratingsMean,c("user_id","venue_id","rating"))
str(ratingsMean)
ratingsMean$user_id<-format(ratingsMean$user_id,scientific = FALSE,trim=T)
ratingsMean$venue_id<-format(ratingsMean$venue_id,scientific = FALSE,trim=T)
ratingsMean$rating<-format(ratingsMean$rating,scientific = FALSE, digit=3,trim=T)
## obtengo los que calificaron como maximo a 100 lugares.
ratingsMean<-read.csv(file=getDataSetPath("ratingsMean.csv"))
names(ratingsMean)<-c("user_id","venue_id","rating")
head(ratingsMean)
str(ratingsMean)
rRMatrix <- as(ratingsMean, "realRatingMatrix")
rRMatrixReducido<- rRMatrix[rowCounts(rRMatrix)>20,]
df<-(as(rRMatrixReducido, "data.frame"))
write.table(df,file=getDataSetPath("ratingsMeanReducido.csv"), row.names = F, col.names= F,sep=",",quote = F)
##obtengo una red social reducida
socialgraphData<-read.csv(getDataSetPath("socialgraph.csv"))
ratingsMeanReducido<-read.csv(file=getDataSetPath("ratingsMeanReducido.csv"),header=F)
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
head(grafoReducidoDF)
ratingsMeansample<-ratingsMean[,.N,by=list(user_id)]
d<-ratingsMeansample[10<N<100,]
hist(d$N, breaks=1000)
head(ratingsMeansample[N<100])
tail(ratingsMeansample)
##guardo sin nombre de columnas por mahout
write.table(ratingsMean,file="foursquare/datasets_csv/ratingsMean.csv", row.names = F, col.names= F,sep=",",quote = F)

