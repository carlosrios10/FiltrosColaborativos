library(igraph)
socialgraphData<-read.csv(getDataSetPath("Csocialgraph.csv"))
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
vertices[which (!is.element(vertices, socialgraphData$first_user_id))]
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

write.table(grafoReducidoDF,file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/redSocialReducida.csv", 
          row.names = F, col.names= F,sep=",",quote = F)


#medidas de centralidad
is.directed(grafo)
degree(grafoReducido,v=c("1"),mode="in")
max(degree(grafoReducido))
min(degree(grafoReducido))

plot(degree.distribution(grafo))
summary(grafo)
adj<-get.adjacency(grafo)
str(adj)
adj[1,1:10]

## Analisis de la red social reducida
redSocialDF<-read.csv(file = getDataSetPath("redSocialReducida.csv"),header = F,
                      colClasses="character");

redSocialDF<-read.csv(file ="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/redSocialReducida.csv",header = F,
                      colClasses="character");

grafo<-graph.data.frame(redSocialDF, directed=T, vertices=NULL)
grafoU<- as.undirected(grafo,mode ="collapse")
write.graph(grafoU,file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/redSocialReducida.",format="pajek")
is.directed(grafo)
degree(grafoU,v="30848")
max(degree(grafo,mode="all"))
min(degree(grafo))
dd<-degree.distribution(grafo)
plot(dd)
boxplot(dd)
bw<-betweenness(grafo)
plot(bw)
boxplot(bw)
which.max(bw)
cc<-closeness(grafo)
plot(cc)
boxplot(cc)
max(cc)
min(cc)
which.max(cc)
0.4082482904638631-0.4
0.00824829>0.01
