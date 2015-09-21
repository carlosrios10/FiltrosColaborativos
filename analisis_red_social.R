library(igraph)
library(stringr)
## Analisis de red social completa.
socialgraphData<-read.csv("datasets/foursquare/datasets_csv/socialgraph2.csv", colClasses="character")
str(socialgraphData)
socialgraphData$first_user_id<-str_trim(socialgraphData$first_user_id)
socialgraphData$second_user_id<-str_trim(socialgraphData$second_user_id)
write.table(socialgraphData,file="datasets/foursquare/datasets_csv/socialgraph2.csv", row.names = F, col.names= T,sep=",",quote = F)
grafoTotal<-graph.data.frame(socialgraphData, directed=T, vertices=NULL)
grafoTotal<- as.undirected(grafoTotal,mode ="collapse")
write.graph(grafoTotal,file="datasets/foursquare/datasets_csv/grafoTotal.graphml",format="graphml")
grafoTotal<-read.graph(file="datasets/foursquare/datasets_csv/grafoTotal.graphml",format="graphml")
d<-degree(grafoTotal)
pr<-page.rank(grafoTotal)
ec<-eigen.centrality(grafoTotal)
ev<-evcent(grafoTotal)
aut<-authority.score(grafoTotal)
bet<-betweenness(grafoTotal)


head(ev)
head(d)
str(d)
head(pr)
str(pr)
pr$vector
head(pr$vector)

df = data.frame(d)
df$pr<-pr$vector
head(df)
degree(grafoTotal,v = "10")
str(s)
pr<-page.rank(grafoTotal,damping = 0.95)$vector
degree<-degree(grafoTotal)
closeness<-closeness(grafoTotal)
betweenness<-betweenness(grafoTotal)
head(pc)
tail(pc)
pc[1,]<-NULL
str(pc)
boxplot(pc$Temperature.Difference.from.100)

boxplot(pr)
boxplot(degree)
boxplot()

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

### mundo pequeño
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
plot(grafoReducido, layout=layout.drl,vertex.size=3, vertex.label=NA)
dev.off()


red.eb.cm <- edge.betweenness.community(grafoReducido)

png(filename="eb.png",width = 1600 , height=900)
plot(grafoReducido, layout=layout.auto,vertex.color=red.eb.cm$membership, edge.arrow.size=0.3,vertex.size=6, vertex.label=NA)
dev.off()

max(neighborhood.size(grafoReducido,order=1))
min(neighborhood.size(grafoReducido,order=1))
mean(neighborhood.size(grafoReducido,order=1))

max(neighborhood.size(grafoReducido,order=2))
min(neighborhood.size(grafoReducido,order=2))
mean(neighborhood.size(grafoReducido,order=2))

## Analisis de red social de users que hicieron check en NY.
grafoNY<-read.graph(file ="datasets/foursquare/datasets_csv/grafo_users_check_NY_UE.graphml",format="graphml")
png(filename="informe-jaiio-2015/figuras/grafo.png",width = 1600 , height=900)
plot(grafoNY, layout=layout.auto, edge.arrow.size=0.3,vertex.size=6, vertex.label=NA)
dev.off()

is.simple(grafoNY)
# cantidad de nodos 
vcount(grafoNY)
# cantidad de aristas
ecount(grafoNY)
# es dirigido
is.directed(grafoNY)
# informacion sobre la conexion de la red.
is.connected(grafoNY, mode="weak")
is.connected(grafoNY, mode="strong")
# Grados
sort(degree(grafoNY, mode="all"))
# Análisis de las distribuciones de grados

plot(degree.distribution(grafoNY, cumulative = FALSE, mode="all"),type="h", xlab="grado", ylab="frecuencia")
points(degree.distribution(grafoNY, cumulative = FALSE, mode="all"))
plot(degree.distribution(grafoNY, cumulative = TRUE, mode="all"), type="l", xlab="grado", ylab="frec. acumulada")

png(filename="informe-jaiio-2015/figuras/ny-rs-distribucion-acum-grados.png",width = 400 , height=400)
plot(degree.distribution(grafoNY, cumulative = TRUE, mode="all"), type="l", xlab="grado", ylab="frec. acumulada")
dev.off()


grados<-degree(grafoNY, mode="all")
resumen.grado<- data.frame(media=mean(grados),
                      max=max(grados), 
                      min=min(grados),
                      var=var(grados),
                      sd=sd(grados))

# El diámetro de la red y quienes participan
diameter(grafoNY, unconnected=F)
diameter(grafoNY, unconnected=T)
get.diameter(grafoNY, unconnected=T)
V(grafoNY)[get.diameter(grafoNY, unconnected=T)]

# intermediacion
betweenness.estimat
# densidad
graph.density(grafoNY)

### mundo pequeño
plf.red <- power.law.fit(degree(grafoNY,mode="all"))
## ¿El parámetro alfa de la función es mayor que 1? ¿Si? OK
plf.red$alpha
## ¿El test de Kologorov-Smirnov es no significativo? OK
plf.red$KS.p
## 
plf.red$xmin

plf.red.2 <- power.law.fit(degree(grafoNY,mode="all"), xmin=7)
plf.red.2$KS.p
## Calculo y testeo de la asortividad
assortativity.degree(grafoNY)
## ¿Cuál es el clique más grande de la red (conexiones no dirigidas)? 
red.lqs <- largest.cliques(grafoNY)
red.lqs[[1]]
V(grafoNY)[red.lqs[[1]]]
red.lc.1 <- induced.subgraph(grafoNY, V(grafoNY)[red.lqs[[1]]])
plot(red.lc.1,vertex.size=6, vertex.label=NA)
### Y ahora buscamos clusters dentro del grafo.
red.wc.cm <- walktrap.community(grafoNY)
V(grafoNY)$grupo <- red.wc.cm$membership

components <- red.wc.cm$membership
colours = sample ( rainbow ( max ( components )+ 1) )
V(grafoNY)$color = colours [ components +1]


png(filename="walktrap2.png",width = 1600 , height=900)
plot(grafoNY, layout=layout.drl,vertex.size=3, vertex.label=NA)
dev.off()


red.eb.cm <- edge.betweenness.community(grafoNY)

png(filename="eb.png",width = 1600 , height=900)
plot(grafoNY, layout=layout.auto,vertex.color=red.eb.cm$membership, edge.arrow.size=0.3,vertex.size=6, vertex.label=NA)
dev.off()

max(neighborhood.size(grafoNY,order=1))
min(neighborhood.size(grafoNY,order=1))
mean(neighborhood.size(grafoNY,order=1))

max(neighborhood.size(grafoNY,order=2))
min(neighborhood.size(grafoNY,order=2))
mean(neighborhood.size(grafoNY,order=2))

###### PageRank ###############
grafoReducido<-read.graph(file ="datasets/foursquare/datasets_csv/NY/grafo_users_check_NY2_UE.graphml",format="graphml")
grafoReducido2<-read.csv(file ="datasets/foursquare/datasets_csv/NY/grafo_users_check_NY2_UE.csv")



                                                                    
head(grafoReducido2)
length(unique(grafoReducido2$to))
plot(grafoReducido)
g2 <- graph( c(1,2,1,4,1,5,2,3,2,5,3,4,3,5), directed=FALSE )
plot(g2)
page.rank(g2,damping = 0.950)$vector
betweenness(g2)
hub.score(g2)$vector
authority.score(g2)$vector
hits

pr<-page.rank(grafoReducido,damping = 0.95)$vector
str(pr)
pr["22"]
1.5569291130174845E-5
9.874839e-06 

head(pr)
which.max(pr) 
hist(pr)
