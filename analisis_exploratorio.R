getwd()
df<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinos.csv",header=T)
hist(df$CantVecinos)
table(df$CantVecinos)
sum(df$CantVecinos)/17778
boxplot(df$CantVecinos)


df2<-read.csv(file="C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/datasets_csv/usuariosVecinos01.csv",header=T)
hist(df2$CantVecinos)
table(df2$CantVecinos)
sum(df2$CantVecinos)/17778
boxplot(df2$CantVecinos)

usuariosVecinos01