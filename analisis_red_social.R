setwd("C:\\Users\\Usuarioç\\Desktop\\carlos\\Tesis")
getwd()
library(igraph)
socialgraphData<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/socialgraph.csv")
ratingsMeanReducido<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/ratingsMeanReducido.csv",
                              header=F)

head(ratingsMeanReducido)
str(ratingsMeanReducido)
length(unique(ratingsMeanReducido$V1))
vertices<-unique(ratingsMeanReducido$V1)
vertices<-as.character(vertices)
str(vertices)
length(vertices)

head(socialgraphData,n = 20)
tail(socialgraphData)
str(socialgraphData)
length(unique(socialgraphData$first_user_id))
length(unique(socialgraphData$second_user_id))
socialgraphData$first_user_id<-as.character(socialgraphData$first_user_id)
socialgraphData$second_user_id<-as.character(socialgraphData$second_user_id)



sum(vertices%notin%socialgraphData$first_user_id)
sum(socialgraphData$first_user_id==vertices)
which (!is.element(vertices, socialgraphData$first_user_id))
vertex<-vertices[which (is.element(vertices, socialgraphData$first_user_id))]



grafo<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
subv <- c("1228393")
socialgraphData$first_user_id==
str(socialgraphData)    

grafoReducido<-induced.subgraph(grafo, vids=vertex)
plot(grafoReducido, layout=layout.fruchterman.reingold, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
grafoReducidoDF<-get.data.frame(grafoReducido, what=c( "edges"))

head(grafoReducidoDF,n=20)
tail(grafoReducidoDF)
str(grafoReducidoDF)

grafoReducidoDF10<-grafoReducidoDF[grafoReducidoDF$from==10,]

#medidas de centralidad
is.directed(grafo)
max(degree(grafo))
plot(degree.distribution(grafo))
summary(grafo)
adj<-get.adjacency(grafo)
str(adj)
adj[1,1:10]



