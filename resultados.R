library(ggplot2)
library(plyr)
library(reshape2)

resultados<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCFKN.csv");
resultadosWF<-melt(resultados,id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));

ggplot(data=resultadosWF, aes(x=Nvecinos, y=value, group=variable, colour=variable)) + 
    geom_line(aes(linetype=variable)) + geom_point(aes(shape=variable),size=4)

resultadosF<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCFKF.csv");
resultadosFWF<-melt(resultadosF,id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));

ggplot(data=resultadosFWF, aes(x=Nvecinos, y=value, group=variable, colour=variable)) + 
    geom_line(aes(linetype=variable)) + geom_point(aes(shape=variable),size=4)

resultadosT<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCFT.csv");
resultadosTW<-melt(resultadosT,id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));

ggplot(data=resultadosTW, aes(x=Threshold, y=value, group=variable, colour=variable)) + 
    geom_line(aes(linetype=variable)) + geom_point(aes(shape=variable),size=4)

resTodos<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCFTodos.csv")
resTodos$Nvecinos<-as.factor(as.character(resTodos$Nvecinos))
resTodos$Nvecinos<-ordered(resTodos$Nvecinos,levels = c("10","30","60","90","125","200"))

resTodosW<-melt(resTodos[1:12,],id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));
resThr<-melt(resTodos[13:18,],id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));

ggplot(data=resTodosW, aes(x=Nvecinos, y=value, group=TVecinos, colour=TVecinos)) + 
    geom_line(aes(linetype=TVecinos)) + geom_point(aes(shape=TVecinos),size=4) +
    facet_grid(variable ~ .)


ggplot(data=resTodosW, aes(x=Nvecinos, y=value)) + 
    geom_bar(aes(fill = TVecinos), stat="identity",position=position_dodge(),size=.3) +# Thinner lines
    facet_grid(variable ~ .) +
    scale_fill_grey(name="Tipo de Vecinos") +      # Set legend title
    xlab("Numero de Vecinos/Amigos") + ylab("Valor") + # Set axis labels
    ggtitle("UBCF") +  # Set title
    theme_bw()
 

ggplot(data=resThr, aes(x=Threshold, y=value)) + 
    geom_bar(colour="black", stat="identity",position=position_dodge(),size=.3) +# Thinner lines
    facet_grid(variable ~ .) +
    scale_fill_hue(name="Tipo de Vecinos") +      # Set legend title
    xlab("Numero de Vecinos/Amigos") + ylab("Valor") + # Set axis labels
    ggtitle("UBCF") +  # Set title
    theme_bw()

ggplot(data=resThr, aes(x=Threshold, y=value, group=variable, colour=variable)) + 
    geom_line(aes(linetype=variable)) + geom_point(aes(shape=variable),size=4)

+
    facet_grid(variable ~ .)