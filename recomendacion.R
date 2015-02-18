setwd("D:\\DOCTORADO\\Proyectos\\Recomendacion\\FiltrosColaborativos")
getwd()
rm(list = ls())
install.packages("recommenderlab")
library("recommenderlab")
library(plyr)
library(data.table)
###lectura del set de datos
ratingsData<-fread("datos/datasets_csv/ratings.csv")
head(ratingsData)
tail(ratingsData)
str(ratingsData)
ratingsMean<-ratingsData[,mean(rating),by=list(user_id,venue_id)]
head(ratingsMean)
names(ratingsMean)<-c("user_id","venue_id","rating")
str(ratingsMean)
write.csv(ratingsMean,file="datos/datasets_csv/ratingsMean.csv", row.names = F, )
ratingsMean<-read.csv(file="datos/datasets_csv/ratingsMean.csv")
head(ratingsMean)
rRMatrix <- as(ratingsMean, "realRatingMatrix")
dim(rRMatrix)
rowCounts(rRMatrix)
##Exploracion de los datos
as(rRMatrix[1,],"list")
rowCounts(rRMatrix[1,])
hist(rowCounts(rRMatrix),breaks=100)
boxplot(rowCounts(rRMatrix))
var(rowCounts(rRMatrix))
sd(rowCounts(rRMatrix))
summary(rowCounts(rRMatrix))
hist(colMeans(rRMatrix))
##Crear un recomendador
userCFModel <- Recommender(rRMatrix, method = "UBCF")
names(getModel(userCFModel))
getModel(userCFModel)$ratings
recomendacion<-NULL
recomendacion<-predict(userCFModel,rRMatrix[1:2],type="rating" )

##evaluar los rating predecidos
e<-evaluationScheme(rRMatrix,method="split",train=0.9,given=1,goodRating=3)


