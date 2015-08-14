library(plyr)
library(ggplot2)
library(lubridate)
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
checkins$dataTime<- ymd_hms(checkins$dataTime)
str(usuariosDistancias)

head(ratingVNY)
head(venues)
head(ratingVNY)
head(checkins)
head(usuariosDistancias)

cantidad_check_user<-ddply(ratingVNY,.(User),summarise,freq=length(User))
cantidad_check_venues<-ddply(ratingVNY,.(Venue),summarise,freq=length(Venue))

head(cantidad_check_user)
table(cantidad_check_user$freq)

head(cantidad_check_venues)
table(cantidad_check_venues$freq)
dist_acumulada<-ecdf(cantidad_check_venues$freq)

ch<-seq(1:500)
ch.p<-dist_acumulada(ch)
plot(ch.p) ## grafico de la distribucion acumulada F(x)
plot(1-ch.p) ## grafico de la distribucio acumulada complementaria 1-F(x)

table(checkins$hour) 
cantidad_check_weekdays<-ddply(checkins,.(isWday,hour),summarize,freq=length(user_id))
str(cantidad_check_weekdays)
cantidad_check_weekdays$hour<-as.numeric(cantidad_check_weekdays$hour)
head(cantidad_check_weekdays)

p <- ggplot(cantidad_check_weekdays, aes(hour, freq))+ geom_line()
p + facet_grid(. ~ wday)


ggplot(checkins, aes(x=dataTime)) + geom_line()

ho<-hour(checkins$dataTime)
dim(checkins[checkins$hour==0 & checkins$isWday==F,])
head(checkins)
ej<-checkins$dataTime[1:3]
ej<-ej[order(ej)]
## ordeno los checkins 
checkins<-checkins[order(checkins$dataTime),]
head(checkins)
inter_checkin_time<-diff(checkins$dataTime)
head(inter_checkin_time)
inter_checkin_time<-inter_checkin_time/60
inter_checkin_time.DF<-ecdf(inter_checkin_time)
inter_checkin_time_sec<-seq(1:1000)

plot(inter_checkin_time.DF(inter_checkin_time_sec))
plot(1-inter_checkin_time.DF(inter_checkin_time_sec))

#user
ratingVNY[ratingVNY$V1==40,]
[416,417,418,419,415]

#vecinos
[33341, 61863, 335485, 20708, 92023, 27785, 76781, 76509, 20788, 31292]

ratingVNY[ratingVNY$V1==33341,]
833
RecommendedItem[item:833, value:4.0], RecommendedItem[item:837, value:3.8586862], RecommendedItem[item:151083, value:3.5454545], RecommendedItem[item:419, value:3.3355079], RecommendedItem[item:99516, value:3.0]]

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
