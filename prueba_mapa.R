library(ggmap)
library(RDSTK)
library(jsonlite)
install.packages('curl')
get_london <- get_map(c(-.137,51.513), zoom=17)
london <- ggmap(get_london)
set.seed(500)
df <- round(data.frame(
        x = jitter(rep(-77.03, 50), amount = .3),#long
        y = jitter(rep( 38.90, 50), amount = .3)#lati
), digits = 2)

df <- data.frame(lat=c(34.048381, 37.757836, 40.729855, 42.356391),
                 lon=c(-118.266164, -122.441033, -73.987921, -71.062307))

res <- lapply(with(df, paste(lat, lon, sep = ",")), geocode, output = "more")
transform(df, city = sapply(res, "[[", "locality"))
r<-geocode("34.048381,-118.266164",output = "more")

map <- get_googlemap("washington",markers =checkins[1:100,c("longitude","latitude")]  ,scale = 4)
ggmap(map, extent = 'device')

json<-coordinates2politics(38.89511, -77.03637)
uu<-fromJSON("https://api.github.com/users/jtleek/repos")
j<-toJSON(uu)
hh<-fromJSON(json)
class(j)
uu$name
hh$politics
strsplit(hh$politics[[1]],NULL)
jj$politics$
j$        
class(jj)
names(j)
head(df)
head(checkins[,c("longitude","latitude")])
df2<-data.frame(x=c(-77.03637),y=c(38.89511))
checkins<-read.csv(file="D:\\DOCTORADO\\Proyectos\\datos\\datasets_csv\\checkins.csv")
head(checkins)
str(checkins)
checkins$created_at<-as.character(checkins$created_at)
checkins$dataTime<-parse_date_time(checkins$created_at,"%y%m%d %H%M%S")
week(checkins$dataTime[1])
str(df)
checkins[1:10,4:5]
###
baylor <- "baylor university"
qmap(baylor, zoom = 14, source = "osm")
tandil <- get_map(location = "tandil")
ggmap(tandil, extent = "normal")
str(crime)
qmap("houston", zoom = 13)
gglocator(2)
violent_crimes <- subset(crime,offense != "auto theft" & offense != "theft" & offense != "burglary")
violent_crimes$offense <- factor(violent_crimes$offense,levels = c("robbery", "aggravated assault", "rape", "murder"))

violent_crimes <- subset(violent_crimes,-95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat <= 29.78400)
theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
HoustonMap + geom_point(aes(x = lon, y = lat, colour = offense, size = offense),data = violent_crimes)
HoustonMap + stat_bin2d(aes(x = lon, y = lat, colour = offense, fill = offense),size = .5, bins = 30, alpha = 1/2,data = violent_crimes)


violent_crimes <- subset(crime,offense != "auto theft" & offense != "theft" & offense != "burglary")

houston <- get_map("houston", zoom = 14)
HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
HoustonMap <- ggmap(houston, extent = "device", legend = "topleft")
HoustonMap + stat_density2d(
                aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                size = 2, bins = 4, data = violent_crimes,
                geom = "polygon"
        )
overlay <- stat_density2d(
        aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
        bins = 4, geom = "polygon",
        data = violent_crimes
)
HoustonMap + overlay + inset(
        grob = ggplotGrob(ggplot() + overlay + theme_inset()),
        xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062
)

##############
setwd("d:/DOCTORADO/Proyectos/")
checkins<-read.csv(file="datasets/foursquare/datasets_csv/checkins.csv")
venues<-read.csv(file="datasets/foursquare/datasets_csv/venues.csv")
users<-read.csv(file="datasets/foursquare/datasets_csv/users.csv")
rating<-read.csv(file="datasets/foursquare/datasets_csv/ratingsMean.csv")
length(unique(rating$user_id))
ratingsUsers<-rating[!duplicated(rating[,c("user_id")]),]
ratingsUsers$venue_id<-NULL
ratingsUsers$rating<-NULL

merge<-merge(ratingsUsers, users,by.x="user_id",by.y="id")
sum(is.na(merge$latitude))
sum(is.na(merge$longitude))
sum(is.na(merge$user_id))
sum(is.na(merge$venue_id))
sum(is.na(merge$rating))
merge1<-merge[complete.cases(merge),]
head(merge1)
km <- kmeans(merge1[,3:4], centers =5 )
plot(merge1$longitude, merge1$latitude, col = km$cluster, pch = 20)
merge1$cluster<-NULL
merge1$cluster<-km$cluster
merge2<-merge1[complete.cases(merge1),]
names(merge2)<-c("user_id","venue_id","rating","lat_user","lon_user","cluster","local_user")
merge3<-merge(merge2, venues,by.x="venue_id",by.y="id")
merge3<-merge3[complete.cases(merge3),]
sum(is.na(merge3$longitude))
merge3[,c("longitude","latitude")]
sum(merge3$longitude>=91.43131 && merge3$latitude<91.43131)      
merge3<-merge3[-472280,]
localVenues<-latlong2state(merge3[,c("longitude","latitude")])
localDf<-data.frame(local=localVenues)
merge3<-cbind(merge3,localDf)
merge3<-merge3[complete.cases(merge3),]
head(merge3)
sum(is.na(merge3$local))
length(unique(merge3$venue_id))

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(merge3,aes(x=longitude, y=latitude)) +   mapWorld
mp <- mp+ geom_point(size=1) 
mp+facet_grid(local ~ .)


library(sp)
library(maps)
library(maptools)
testPoints <- data.frame(x = merge1$longitude, y = merge1$latitude)
localt<-latlong2state(testPoints)
df<-data.frame(local=localt)
head(merge1)
merge1<-cbind(merge1,df)
table(df$local)
mp+geom_density2d()

mp + stat_density2d(
        aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
        size = 2, bins = 4, data = merge,
        geom = "polygon"
)
str(violent_crimes)
