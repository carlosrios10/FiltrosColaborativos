getDataSetPath<-function(dataSetName){
        tipo<-strsplit(dataSetName,"[.]")[[1]][2];
        if(tipo=="dat"){ 
                return (paste(wd,"/datos/umn_foursquare_datasets/",dataSetName,sep="/"))
        }
        else{
                return (paste(wd,"/datasets/foursquare/datasets_csv/",dataSetName,sep="/"))
        }
}

###
getLocality<-function(dfLatLon){
        resultado<-data.frame()
        for(i in 1:nrow(dfLatLon)){
                lat<-dfLatLon[i,"latitude"]
                lon<-dfLatLon[i,"longitude"]
                if((is.na(lat) || is.na(lon))){
                        geo<-data.frame(address=NA,locality=NA,country=NA)        
                }else{
                        geo<-geocode(paste(lat, lon, sep = ","),output = "more")  
                }
                resultado<-rbind(resultado,geo[,c("address","locality","country")])
        }
        return (resultado)
        
}

geo.dist = function(df) {
        require(geosphere)
        d <- function(i,z){         # z[1:2] contain long, lat
                dist <- rep(0,nrow(z))
                dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
                return(dist)
        }
        dm <- do.call(cbind,lapply(1:nrow(df),d,df))
        return(as.dist(dm))
}

getLocality2<-function(dfLatLon){
        resultado<-data.frame()
        for(i in 1:nrow(dfLatLon)){
                print(i)
                lat<-dfLatLon[i,"latitude"]
                lon<-dfLatLon[i,"longitude"]
                if((is.na(lat) || is.na(lon))){
                        geo<-data.frame(countryCode=NA,countryName=NA)        
                }else{
                        
                        list <- tryCatch(
                        {GNcountryCode(lat,lon)
                        },
                        error=function(cond) {
                                message("Here's the original error message:")
                                message(cond)
                                # Choose a return value in case of error
                                return(data.frame(countryCode=NA,countryName=NA))
                        },
                        warning=function(cond) {
                                message("Here's the original warning message:")
                                message(cond)
                                return(NULL)
                        },
                        finally={
                                message("Some other message at the end")
                        }
                        )    
                        geo<-data.frame(countryCode=ff$countryCode,countryName=ff$countryName) 
                }
                resultado<-rbind(resultado,geo)
        }
        return (resultado)
        
}
####onbtiene ciudad de estados unidos dado lon y lat
latlong2state <- function(pointsDF) {
        # Prepare SpatialPolygons object with one SpatialPolygon
        # per state (plus DC, minus HI & AK)
        states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
        IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
        states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                         proj4string=CRS("+proj=longlat +datum=wgs84"))
        
        # Convert pointsDF to a SpatialPoints object 
        pointsSP <- SpatialPoints(pointsDF, 
                                  proj4string=CRS("+proj=longlat +datum=wgs84"))
        
        # Use 'over' to get _indices_ of the Polygons object containing each point 
        indices <- over(pointsSP, states_sp)
        
        # Return the state names of the Polygons object containing each point
        stateNames <- sapply(states_sp@polygons, function(x) x@ID)
        stateNames[indices]
}
