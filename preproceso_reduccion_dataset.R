library("recommenderlab")
library("igraph")
### Reduccion de los rating.
ratingsMean<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv")
rRMatrix <- as(ratingsMean, "realRatingMatrix")
rRMatrixReducido<- rRMatrix[rowCounts(rRMatrix)>20,]
ratingsMeanReducido<-(as(rRMatrixReducido, "data.frame"))
write.table(ratingsMeanReducido,file="datasets/foursquare/datasets_csv/ratingsMeanReducido.csv", row.names = F, col.names= F,sep=",",quote = F)
### Reduccion de la red social.
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
### Reduccion de rating solo a usuarios y lugares de  estados unidos
library(sp)
library(maps)
library(maptools)
venues<-read.csv(file="datasets/foursquare/datasets_csv/venues.csv")
users<-read.csv(file="datasets/foursquare/datasets_csv/users.csv")
rating<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv")
length(unique(rating$user_id))
ratingsUsers<-rating[!duplicated(rating[,c("user_id")]),]
ratingsUsers$venue_id<-NULL
ratingsUsers$rating<-NULL

