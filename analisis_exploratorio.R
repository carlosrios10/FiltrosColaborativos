library(plyr)
library(ggplot2)
library(reshape2)
### Analisis de los ratings.
ratings<-read.csv(file = "C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/ratingsMeanReducido.csv",header=F);
head(ratings)
names(ratings)<-c("user","item","rating")
## Cantidad de usuario y de items.
str(ratings)
length(unique(ratings$user))
length(unique(ratings$item))
mResumen<- data.frame(media=mean(ratings$rating),
                      max=max(ratings$rating), 
                      min=min(ratings$rating),
                      var=var(ratings$rating),
                      sd=sd(ratings$rating))
mResumen
ggplot(ratings,aes(x=factor(0),y=rating) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(ratings,aes(x=rating))+geom_histogram(binwidth=.5)

### cuantos lugares visitó cada usuario
visitas<-ddply(ratings,.(user), summarise, visitas=length(item))
visitas<-visitas[order(visitas$visitas,decreasing = T),]
#los 5 primeros usuarios con mas visitas.
head(visitas)
tail(visitas)
vResumen<- data.frame(media=mean(visitas$visitas),
                      max=max(visitas$visitas), 
                      min=min(visitas$visitas),
                      var=var(visitas$visitas),
                      sd=sd(visitas$visitas))

vResumen
## Distribucion de las visitas realizadas por los usuarios.
ggplot(visitas,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(visitas, aes(x=visitas)) + geom_density() +scale_x_continuous(breaks = seq(20, 500, by = 100)) 

### los lugares cuantas veces fueron visitados.
lugares<-ddply(ratings,.(item), summarise, visitas=length(user), 
               totalRating=sum(rating))
lugares<-lugares[order(lugares$visitas,decreasing = T),]
head(lugares)
tail(lugares)
lResumen<- data.frame(media=mean(lugares$visitas),
                      max=max(lugares$visitas), 
                      min=min(lugares$visitas),
                      var=var(lugares$visitas),
                      sd=sd(lugares$visitas))

lResumen
##Distribucion de las cantidades de veces que fueron visitados los lugares.
ggplot(lugares,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(lugares, aes(x=visitas)) + geom_density() +scale_x_continuous(breaks = seq(1, 500, by = 100)) 
## Frecuencia de las cantidad de visitas.
visitCant<-as.data.frame(table(lugares$visitas), stringsAsFactors=TRUE)
names(visitCant)<-c("CantVisitas","Freq")
head(visitCant)
ggplot(visitCant, aes(x=(CantVisitas),y=Freq)) + geom_histogram(stat="identity") 

## Solapamiento
solapamiento<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/solapFreq.csv")
solapamiento
ggplot(solapamiento, aes(x=as.factor(overlap),y=frecuencia)) + geom_histogram(stat="identity") 



path<-"C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa"
file<-paste(path,"/",1,".csv",sep="")    
solapamiento<-read.csv(file)
for(i in 2:17778){
    print(i)
    file<-paste(path,"/",i,".csv",sep="");    
    df<-read.csv(file);
    solapamiento<-rbind(solapamiento,df);    
}
head(solapamiento)
combinatoria<-function(valor){
    return (dim(combn(valor, 2))[2])
}
(17778*17778)/2
combinatoria(17778)
combn(17778,2)
overlap<-lapply(lugares$visitas[lugares$visitas>1],combinatoria)
overlapdf <-data.frame(overlap = unlist(overlap))
lugares$overlap[lugares$visitas==1]<-0
lugares$overlap[lugares$visitas>1]<-overlapdf[,1]
max(lugares$overlap)
min(lugares$overlap)
max(lugares$overlap)/30
str(lugares)
head(lugares)
tail(lugares,n=20)
table(lugares$overlap)
f<-countdf <- as.data.frame(table(lugares$overlap), stringsAsFactors=TRUE)
ggplot(data=f, aes(x=Var1, y=Freq)) + geom_bar(stat="identity")
summary(lugares)
sum(f$Freq)
lugares$visitas-min(lugares$visitas)/(max(lugares$visitas)-min(lugares$visitas))
lugares$visitasN<-(lugares$visitas-min(lugares$visitas))/diff(range(lugares$visitas))
lugares$visitasS<-scale(lugares[,2])
mean(lugares$visitasS)
sd(lugares$visitasS)
head(lugares)
range(lugares$visitas)
###Analisis Cantidad de Vecinos.
### vecinos calculados con la correlacion Pearson.

usuariosVecinos<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinos.csv",header=T)
head(usuariosVecinos)
tail(usuariosVecinos)
vecinosP<-ddply(usuariosVecinos,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0) )

### vecinos calculados con la distancia Euclidea.
usuariosVecinosE<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosE.csv",header=T)
head(usuariosVecinosE)
tail(usuariosVecinosE)
ddply(usuariosVecinosE,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0) )

### vecinos calculados con la similitud coseno.
usuariosVecinosC<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosC.csv",header=T)
head(usuariosVecinosC)
tail(usuariosVecinosC)
vecinosC<-ddply(usuariosVecinosC,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0))

### vecinos calculados con la similitud tanimoto.
usuariosVecinosT<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinosT.csv",header=T)
head(usuariosVecinosT)
tail(usuariosVecinosT)
ddply(usuariosVecinosT,.(Threshold),summarise,mean=mean(CantVecinos), total= sum(CantVecinos),totalCeros=sum(CantVecinos==0))

##################################
##########Analizando checkins#####
getwd()
setwd("D:\\DOCTORADO\\Proyectos\\Recomendacion\\FiltrosColaborativos")
checkins<-read.csv(file="D:\\DOCTORADO\\Proyectos\\datos\\datasets_csv\\checkins.csv")
head(checkins)
str(checkins)
checkins$created_at<-as.character(checkins$created_at)
checkins$dataTime<-parse_date_time(checkins$created_at,"%y%m%d %H%M%S")
week(checkins$dataTime[1])

