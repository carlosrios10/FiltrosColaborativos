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
## Analisis de red social completa.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph.csv", colClasses="character")
head(socialgraphData)
str(socialgraphData)
grafo<-graph.data.frame(socialgraphData,directed = T, vertices = NULL)
grafoU<- as.undirected(grafo,mode ="collapse")
27098472/2
user<-read.csv("datasets/foursquare/datasets_csv/users.csv")
head(user)
g2 <- graph( c(1,2,2,3,3,4,5,6), directed=T )
plot(g2)
str(user)
length(unique(user$id))


write.graph(grafoU,file="datasets/foursquare/datasets_csv/redSocial.graphml",format="graphml")

## Analisis de la red social reducida
redSocialDF<-read.csv(file ="datasets/foursquare/datasets_csv/redSocialReducida.csv",header = F,colClasses="character");
grafoReducido<-read.graph(file ="datasets/foursquare/datasets_csv/redSocialReducida.graphml",format="graphml")
grafo<-graph.data.frame(redSocialDF, directed=T, vertices=NULL)
grafoU<- as.undirected(grafo,mode ="collapse")
write.graph(grafoU,file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/redSocialReducida.",format="pajek")
is.simple(grafoReducido)
# cantidad de nodos 
vcount(grafoReducido)
# cantidad de aristas
ecount(grafoReducido)
# es dirigido
is.directed(grafoReducido)
# informacion sobre la conexion de la red.
is.connected(grafoReducido, mode="weak")
is.connected(grafoReducido, mode="strong")
# Grados
sort(degree(grafoReducido, mode="all"))
# Análisis de las distribuciones de grados

plot(degree.distribution(grafoReducido, cumulative = FALSE, mode="all"),type="h", xlab="grado", ylab="frecuencia")
points(degree.distribution(grafoReducido, cumulative = FALSE, mode="all"))
plot(degree.distribution(grafoReducido, cumulative = TRUE, mode="all"), type="l", xlab="grado", ylab="frec. acumulada")

# El diámetro de la red y quienes participan
diameter(grafoReducido, unconnected=F)
diameter(grafoReducido, unconnected=T)
get.diameter(grafoReducido, unconnected=T)
V(grafoReducido)[get.diameter(grafoReducido, unconnected=T)]

# intermediacion
betweenness.estimat
# densidad
graph.density(grafoReducido)

###################### mundo pequeño
plf.red <- power.law.fit(degree(grafoReducido,mode="all"))
## ¿El parámetro alfa de la función es mayor que 1? ¿Si? OK
plf.red$alpha
## ¿El test de Kologorov-Smirnov es no significativo? OK
plf.red$KS.p
## 
plf.red$xmin

plf.red.2 <- power.law.fit(degree(grafoReducido,mode="all"), xmin=7)
plf.red.2$KS.p
## Calculo y testeo de la asortividad
assortativity.degree(grafoReducido)
## ¿Cuál es el clique más grande de la red (conexiones no dirigidas)? 
red.lqs <- largest.cliques(grafoReducido)
red.lqs[[1]]
V(grafoReducido)[red.lqs[[1]]]
red.lc.1 <- induced.subgraph(grafoReducido, V(grafoReducido)[red.lqs[[1]]])
plot(red.lc.1,vertex.size=6, vertex.label=NA)
### Y ahora buscamos clusters dentro del grafo.
red.wc.cm <- walktrap.community(grafoReducido)
V(grafoReducido)$grupo <- red.wc.cm$membership

components <- red.wc.cm$membership
colours = sample ( rainbow ( max ( components )+ 1) )
V(grafoReducido)$color = colours [ components +1]


png(filename="walktrap2.png",width = 1600 , height=900)
plot(grafoReducido, layout=layout,vertex.size=3, vertex.label=NA)
dev.off()


red.eb.cm <- edge.betweenness.community(grafoReducido)

png(filename="eb.png",width = 1600 , height=900)
plot(grafoReducido, layout=layout.auto,vertex.color=red.eb.cm$membership, edge.arrow.size=0.3,vertex.size=6, vertex.label=NA)
dev.off()

str(grafoReducido)
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
