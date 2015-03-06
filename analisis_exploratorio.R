library(plyr)
library(ggplot2)

### Analisis de los ratings.
ratings<-read.csv(file = getDataSetPath("ratingsMeanReducido.csv"),header=F);
names(ratings)<-c("user","item","rating")
mResumen<- data.frame(media=mean(ratings$rating),
                      max=max(ratings$rating), 
                      min=min(ratings$rating),
                      var=var(ratings$rating),
                      sd=sd(ratings$rating))

ggplot(ratings,aes(x=factor(0),y=rating) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

ggplot(ratings,aes(x=rating))+geom_histogram(binwidth=.5)
table(ratings$rating)

theme_remove_all <- theme(axis.text = element_blank(),
                          axis.title = element_blank(),
                          axis.ticks =  element_blank(),
                          axis.ticks.margin = unit(0, "lines"),
                          axis.ticks.length = unit(0, "cm"))

ggplot(ratings, aes(x = factor(1), y = rating)) + 
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(position = position_jitter(width = 0.05)) +
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = c(min(ratings$rating) - 0.1 * diff(range(ratings$rating)), 
                        max(ratings$rating) + 0.1 * diff(range(ratings$rating)))) + 
    coord_flip() 


### cuantos lugares visitó cada usuario
head(ratings)
visitas<-ddply(ratings,.(user), summarise, visitas=length(item))
head(visitas)
vResumen<- data.frame(media=mean(visitas$visitas),
                      max=max(visitas$visitas), 
                      min=min(visitas$visitas),
                      var=var(visitas$visitas),
                      sd=sd(visitas$visitas))

vResumen

ggplot(visitas,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    geom_boxplot(outlier.shape=NA)+
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")

### los lugares cuantas veces fueron visitados.
lugares<-ddply(ratings,.(item), summarise, visitas=length(user), totalRating=sum(rating))
head(lugares)
lResumen<- data.frame(media=mean(lugares$visitas),
                      max=max(lugares$visitas), 
                      min=min(lugares$visitas),
                      var=var(lugares$visitas),
                      sd=sd(lugares$visitas))

lResumen

ggplot(lugares,aes(x=factor(0),y=visitas) )+ 
    geom_boxplot()+ 
    coord_flip()+  
    stat_summary(fun.y=mean, geom="point")


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


par(mfrow=c(2,2))

uno = cbind(anscombe[1],anscombe[5])
attach(uno)
plot(x1, y1, xlim = c(3, 19), ylim = c(3, 13))
abline(lm(y1~x1))

dos = cbind(anscombe[2],anscombe[6])
attach(dos)
plot(x2, y2, xlim = c(3, 19), ylim = c(3, 13))
abline(lm(y2~x2))

tres = cbind(anscombe[3],anscombe[7])
attach(tres)
plot(x3, y3, xlim = c(3, 19), ylim = c(3, 13))
abline(lm(y3~x3))

cuatro = cbind(anscombe[4],anscombe[8])
attach(cuatro)
plot(x4, y4, xlim = c(3, 19), ylim = c(3, 13))
abline(lm(y4~x4))


theme_set(theme_bw(base_size=18))

anscombe_m <- data.frame()

for(i in 1:4)
    anscombe_m <- rbind(anscombe_m, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))

ggplot(anscombe_m, aes(x, y)) + geom_point(size=5, color="red", fill="orange", shape=21) + geom_smooth(method="lm", fill=NA, fullrange=TRUE) + facet_wrap(~set)
