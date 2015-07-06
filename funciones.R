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
#### fubcion para onbtener rating de una ciudad de estados unidos
earth.dist <- function (long1, lat1, long2, lat2)
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

################## calcula la distancia entre dos puntos gps

geodetic.distance <- function(point1, point2)
{
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))    
    d <- acos(d)
    R*d
}



