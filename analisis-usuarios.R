library(plyr)
getwd()
rating <- read.csv(file = "datasets/foursquare/datasets_csv/NY/rating_NY_2_mas_check_UE.csv",header=F)
venues <- read.csv(file = "datasets/foursquare/datasets_csv/NY/venues_check_NY_2_mas_check_UE.csv",header=T)
names(rating )<-c("User","Item","Rating")
head(rating)
str(rating)
userio.media<- ddply(rating,.(User),summarize,mean=mean(Rating),cantItem=length(Item),sd=sd(Rating))
head(userio.media,n=100)
boxplot(userio.media$mean)
hist(userio.media$mean)
table((userio.media$cantItem))
std()
hist(userio.media$sd)
rating[rating$User==1540,]

head(venues)
venues[venues$V1=="21646",]
userio.media
30.267256
-97.743568

40.721469279859235
-73.98853826098953

40.71454
-73.992073

venuesDetalles<-read.csv(file = "datasets/foursquare/datasets_csv/NY/detalles2_venues_check_NY_2_mas_check_UE.csv",header=F,sep="\t")

venuesCercanas<-read.csv(file = "datasets/foursquare/datasets_csv/NY/venuesfromlatlon_venues_check_NY_2_mas_check_UE.csv",header=F,sep="\t")
venuesCercanas<- venuesCercanas[order(venuesCercanas$V1,venuesCercanas$V11),] 

write.csv(x = venuesCercanas,file = "")


