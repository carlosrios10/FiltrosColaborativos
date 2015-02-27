setwd("C:\\Users\\Usuarioç\\Desktop\\carlos\\Tesis\\workspaceR\\Recomendacion\\FiltrosColaborativos")
getwd()
rm(list = ls())
install.packages("recommenderlab")
install.packages("data.table")
library("recommenderlab")
library(plyr)

library(Hmisc)
###lectura del set de datos
ratingsMean<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/ratingsMean.csv")
names(ratingsMean)<-c("user_id","venue_id","rating")
head(ratingsMean)
str(ratingsMean)
rRMatrix <- as(ratingsMean, "realRatingMatrix")

##Exploracion de los datos
rRMatrix # 661986 usuarios x 1140494 items rating matrix with 2436722 ratings.

#Analizamos las distribuciones de los ratings 
head(getRatings(rRMatrix))
sdDF<- data.frame(mean=mean(getRatings(rRMatrix)),
                  sd=sd(getRatings(rRMatrix)),
                  var=var(getRatings(rRMatrix)),
                  max=max(getRatings(rRMatrix)),
                  min=min(getRatings(rRMatrix))                  
                  )

sdDF #medidas resumenes de los ratings

ggplot(ratingsMean, aes(x=rating)) + geom_histogram(binwidth=1, colour="black", fill="white")+
    geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=0.1)

#se observa que existen muchos rating valor 2 y 5.

#Normalizamos los datos centrando los datos restando la media
hist(getRatings(normalize(rRMatrix)), breaks=100)
#Normalizamos los datos centrando los datos restando la media y dividiendo por el sd
hist(getRatings(normalize(rRMatrix, method="Z-score")), breaks=100)
#Vamos a ver a cuantos lugares los usuarios han calificado 
hist(rowCounts(rRMatrix), breaks=1000)
boxplot(rowCounts(rRMatrix))
max(rowCounts(rRMatrix))
min(rowCounts(rRMatrix))
mean(rowCounts(rRMatrix))
sum(rowCounts(rRMatrix)==1)

#vamos a ver los items que fueron calificados
hist(colCounts(rRMatrix), breaks=1000)
boxplot(colCounts(rRMatrix))
max(colCounts(rRMatrix))
min(colCounts(rRMatrix))
mean(colCounts(rRMatrix))
sum(colCounts(rRMatrix)==0)


#me quedo con los que calificaron a mas de 20 lugares, me quedan 17778 usuarios, 673569 ratings .
rRMatrixReducido<- rRMatrix[rowCounts(rRMatrix)==150,]
#exploramos el nuevo data set
hist(rowCounts(rRMatrixReducido), breaks=1000)
boxplot(rowCounts(rRMatrixReducido))
max(rowCounts(rRMatrixReducido))
min(rowCounts(rRMatrixReducido))
mean(rowCounts(rRMatrixReducido))



df<-(as(rRMatrixReducido, "data.frame"))
write.table(df,file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/ratingsMeanReducido7.csv", row.names = F, col.names= F,sep=",",quote = F)

##Crear un recomendador
userCFModel <- Recommender(rRMatrix, method = "UBCF")
names(getModel(userCFModel))
getModel(userCFModel)$ratings
recomendacion<-NULL
recomendacion<-predict(userCFModel,rRMatrix[1:2],type="rating" )

##evaluar los rating predecidos
e<-evaluationScheme(rRMatrixReducido,method="split",train=0.9,given=1,goodRating=3)
r1 <- Recommender(getData(e, "train"), "UBCF")
r1
r2 <- Recommender(getData(e, "train"), "IBCF")
r2