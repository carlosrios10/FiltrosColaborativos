getwd()
solpamiento<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/cantidadSolpamientoPorUsuarioLiked.csv",sep="\t",header = F)
str(solpamiento)
head(solpamiento)
table(solpamiento$V2)
solpamientoLH<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/cantidadSolpamientoPorUsuarioLikedHated.csv",sep="\t",header = F)
head(solpamientoLH)
table(solpamientoLH$V2)

solpamiento<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosSolapa/cantidadSolpamientoPorUsuarioLiked.csv",sep="\t",header = F)





