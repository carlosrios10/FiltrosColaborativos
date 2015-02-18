# Content of Files
# users.dat: consists of a set of users such that each user has a unique id and a geospatial location (latitude and longitude) that represents the user home town location.
# venues.dat: consists of a set of venues (e.g., restaurants) such that each venue has a unique id and a geospatial location (lattude and longitude).
# checkins.dat: marks the checkins (visits) of users at venues. Each check-in has a unique id as well as the user id and the venue id.
# socialgraph.dat: contains the social graph edges (connections) that exist between users. Each social connection consits of two users (friends) represented by two unique ids (first_user_id and second_user_id).
# ratings.dat: consists of implicit ratings that quantifies how much a user likes a specific venue.

readLines("umn_foursquare_datasets/venues.dat",n=2)
venuesData <- read.delim('umn_foursquare_datasets/venues.dat', header = F, sep="|",skip=2) 
names(venuesData)<-c("id","latitude","longitude")
head(venuesData)
tail(venuesData)
dim(venuesData)
venuesData<-venuesData[-1143091,]
write.csv(venuesData,file="datasets_csv/venues.csv", row.names = F)

readLines("umn_foursquare_datasets/socialgraph.dat",n=2)
socialData<-read.delim('umn_foursquare_datasets/socialgraph.dat', header = F, sep="|",skip=2) 
names(socialData)<-c("first_user_id","second_user_id")
head(socialData)
tail(socialData,n=100)
socialData<-socialData[-27098473:-27098488,]
write.csv(socialData,file="datasets_csv/socialgraph.csv", row.names = F)

readLines("umn_foursquare_datasets/users.dat",n=2)
usersData<-read.delim('umn_foursquare_datasets/users.dat', header = F, sep="|",skip=2) 
names(usersData)<-c("id", "latitude", "longitude")
head(usersData)
tail(usersData)
usersData<-usersData[-2153470,]
write.csv(usersData,file="datasets_csv/users.csv", row.names = F)

readLines("umn_foursquare_datasets/checkins.dat",n=2)
checkinsData<-read.delim('umn_foursquare_datasets/checkins.dat', header = F, sep="|",skip=2) 
names(checkinsData)<-c("id","user_id","venue_id","latitude","longitude","created_at")
head(checkinsData)
tail(checkinsData)
checkinsData<-checkinsData[-1021967,]
write.csv(checkinsData,file="datasets_csv/checkins.csv", row.names = F)

readLines("datos/umn_foursquare_datasets/ratings.dat",n=2)
ratingsData<-read.delim('datos/umn_foursquare_datasets/ratings.dat', header = F, sep="|",skip=2) 
names(ratingsData)<-c("user_id","venue_id","rating")
head(ratingsData)
tail(ratingsData)
ratingsData<-ratingsData[-2809581,]
write.csv(ratingsData,file="datos/datasets_csv/ratings.csv", row.names = F, )
