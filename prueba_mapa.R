### Mapa UE.
library(ggplot2)
checkinsUE<-read.csv("datasets/foursquare/datasets_csv/checkinsUE.csv")
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(checkinsUE,aes(x=lon_venue, y=lat_venue)) +   mapWorld
mp <- mp+ geom_point(size=1) 
png(filename="mapa_venues_ue.png",width = 1600 , height=900)
mp
dev.off()

