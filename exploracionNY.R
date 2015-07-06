ratingVNY<-read.csv(file="datasets/foursquare/datasets_csv/NY/rating_NY2_UE.csv",header=F)
venues<-read.csv(file="datasets/foursquare/datasets_csv/NY/venues_check_NY2_UE.csv",colClass="character")
users<-read.csv(file="datasets/foursquare/datasets_csv/NY/users_check_NY2_UE.csv",colClass="character")
checkins<-read.csv(file="datasets/foursquare/datasets_csv/NY/checkins_check_NY_UE.csv",colClass="character")
usuariosDistancias<-read.csv(file="datasets/foursquare/resultados/usuarios_distancias.csv")

names(ratingVNY)<-c("User","Venue","Rating")
str(ratingVNY)
str(venues)
str(users)
str(checkins)
str(usuariosDistancias)

head(ratingVNY)
head(venues)
head(ratingVNY)
head(checkins)
head(usuariosDistancias)
#### Agregar distancias a los usuarios ####
hist(usuariosDistancias$cantidadDeItem)
hist(usuariosDistancias$distanciaPromedioRecorrida)
hist(usuariosDistancias$distanciaTotalEntreItems)



checkins[checkins$user_id==1408713,]
length(unique(ratingVNY$User))

users[users$User==1095712,]
sum(!duplicated(users$longitudeUser))

dist<-read.csv(file="datasets/foursquare/datasets_csv/usuariosSolapa/distanciaskm.csv",colClass="character")
head(dist)
str(dist)
dist$Similitud<-as.numeric(dist$Similitud)
hist(dist$Similitud)
boxplot(dist$Similitud)
head(dist[dist$Similitud>2,])
users[users$User==40,]

users[users$User==22,]
users[users$User==33,]
users[users$User== 47,]

library(combinat)
dim(combn(100, 2))

dd<-ddply(ratingVNY,.(User),summarize, freq=length(User))
head(dd)
ratingVNY[ratingVNY$User==38,]
hist(dd$freq)
summary(dd$freq)
boxplot(dd$freq)
table(dd$freq)
34.0522342  -118.2436849
42.8353641   -71.6489604
37.09024    -95.712891
geodetic.distance(c(34.0522342,-118.2436849),c(37.09024   , -95.712891)) 
1/5165.758
1/2506.41
