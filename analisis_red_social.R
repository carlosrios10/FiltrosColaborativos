setwd("C:\\Users\\Usuarioç\\Desktop\\carlos\\Tesis")
getwd()
library(igraph)
usersData<-read.csv("datasets_csv/users.csv")
venuesData<-read.csv("datasets_csv/venues.csv")
checkinsData<-read.csv("datasets_csv/checkins.csv")
socialgraphData<-read.csv("datasets_csv/socialgraph.csv")
ratingsData<-read.csv("datasets_csv/ratings.csv",colClasses=c("numeric","numeric","numeric"))
ratingsData$user_id<-format(ratingsData$user_id,scientific = FALSE,trim=T)
ratingsData$venue_id<-format(ratingsData$venue_id,scientific = FALSE,trim=T)
write.table(ratingsData,file="datasets_csv/ratings2.csv", row.names = F, col.names= F,sep=",",quote = F)

head(ratingsData)
str(ratingsData)
boxplot(ratingsData$venue_id)
table(ratingsData$venue_id)
4e+05

grafo<-graph.data.frame(socialgraphData, directed=F, vertices=NULL)
#medidas de centralidad
is.directed(grafo)
max(degree(grafo))
plot(degree.distribution(grafo))
summary(grafo)
adj<-get.adjacency(grafo)
str(adj)
adj[1,1:10]


aa<-socialgraphData[1:100,]

aalista<-as.list(aa)

g <- graph.ring(10)
get.adjlist(g)
get.adjedgelist(g)
str(grafo)
vec<-as.numeric(as.vector(socialgraphData[1,]))
str(vec[[1]])
data.m
v<-socialgraphData[[1,1:2]]
str(v)
