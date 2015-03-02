getDataSetPath<-function(dataSetName){
        tipo<-strsplit(dataSetName,"[.]")[[1]][2];
        if(tipo=="dat"){ 
                return (paste(wd,"/datos/umn_foursquare_datasets/",dataSetName,sep="/"))
        }
        else{
                return (paste(wd,"/datasets/foursquare/datasets_csv/",dataSetName,sep="/"))
        }
}