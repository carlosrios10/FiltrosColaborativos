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

### Voy a mostrar Coseno y Pearson.
### completar los Nvecinos de Threshold.

resultadoPC<-resTodos[resTodos$Similitud=="COSENO"|resTodos$Similitud=="PEARSON" ,]
resultadoPC[resultadoPC$TVecinos=="THRESHOLD"&resultadoPC$Similitud=="PEARSON",3]<-vecinosP$mean
resultadoPC[resultadoPC$TVecinos=="THRESHOLD"&resultadoPC$Similitud=="COSENO",3]<-vecinosC$mean

resTodosW<-melt(resTodos[1:12,],id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));
resThr<-melt(resTodos[13:18,],id.vars = c("Similitud", "TVecinos","Nvecinos","Threshold"));

ggplot(data=resultadoPC, aes(x=Nvecinos, y=Mae, group=TVecinos, colour=TVecinos)) + 
    geom_line(aes(linetype=TVecinos)) + 
    geom_point(aes(shape=TVecinos),size=3) +
    xlim(-10, 280)+
    facet_grid(Similitud ~ .)


ggplot(data=resultadoPC, aes(x=Nvecinos, y=Mae)) + 
    geom_bar(aes(fill = TVecinos), stat="identity",position=position_dodge(),size=.10) +# Thinner lines
    facet_grid(Similitud ~ .) +
    scale_fill_grey(name="Tipo de Vecinos") +      # Set legend title
    xlab("Numero de Vecinos/Amigos") + ylab("MAE") + # Set axis labels
    ggtitle("UBCF") +  # Set title
    theme_bw()

ggplot(data=resultadoPC, aes(x=Nvecinos, y=Mae)) + 
    geom_vline+
    facet_grid(Similitud ~ .) +
    scale_fill_grey(name="Tipo de Vecinos") +      # Set legend title
    xlab("Numero de Vecinos/Amigos") + ylab("MAE") + # Set axis labels
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
+    facet_grid(variable ~ .)

### Resultados segun similitud estructural.
simEstr<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCFTodosSimEstr.csv")
ggplot(data=simEstr, aes(x=as.factor(Threshold),y=Mae,fill=Similitud))+ 
    geom_bar(stat="identity",position=position_dodge())

#### REsultados JAIIO
ratingReducido.result<-read.csv("datasets/foursquare/resultados/resultadosUBCFTodosJAIIO.csv")
comparacion<-read.csv("datasets/foursquare/resultados/comparacion_datos_reducidos.csv",sep = ";")

ratingReducido.result$metodo<-paste(ratingReducido.result$Similitud,
                                    ratingReducido.result$TVecinos,
                                    ratingReducido.result$Nvecinos,
                                    ratingReducido.result$Threshold,sep="-")

ratingReducido.result<-ratingReducido.result[,c("metodo","Mae","Rms")]
str(ratingReducido.result)
ratingReducido.result$metodo[1]<-"C-T-06"
ratingReducido.result$metodo[2]<-"C-T-07"
ratingReducido.result$metodo[3]<-"C-T-08"
ratingReducido.result$metodo[4]<-"C-T-09"
ratingReducido.result$metodo[5]<-"C-KF-10"
ratingReducido.result$metodo[6]<-"C-kF-60"
ratingReducido.result$metodo[7]<-"C-kF-120"
ratingReducido.result$metodo[8]<-"C-KF-ALL"
ratingReducido.result$metodo[9]<-"C-KFF-10"
ratingReducido.result$metodo[10]<-"C-KFF-60"
ratingReducido.result$metodo[11]<-"C-KFF-120"
ratingReducido.result$metodo[12]<-"C-KFF-ALL"
ratingReducido.result$metodo[13]<-"S-T-06"
ratingReducido.result$metodo[14]<-"S-T-07"
ratingReducido.result$metodo[15]<-"S-T-08"
ratingReducido.result$metodo[16]<-"S-T-09"

ratingReducido.result$metodo<-as.factor(ratingReducido.result$metodo)
ratingReducido.result$metodo <- factor(ratingReducido.result$metodo, levels = ratingReducido.result$metodo[order(ratingReducido.result$Mae)])

ggplot(data=ratingReducido.result, aes(x=metodo,y=Mae))+ 
    geom_bar(stat="identity",position=position_dodge())+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


### NY
resultadoNy<-read.csv("datasets/foursquare/resultados/resultadosUBCF_NY_UE.csv")
head(resultadoNy)
resultadoNy$metodo<-paste(resultadoNy$Similitud,
                          resultadoNy$TVecinos,
                          resultadoNy$Nvecinos,
                          resultadoNy$Threshold,sep="-")


resultadoNy$metodo[1]<-"C-T-06"
resultadoNy$metodo[2]<-"C-T-07"
resultadoNy$metodo[3]<-"C-T-08"
resultadoNy$metodo[4]<-"C-T-09"
resultadoNy$metodo[5]<-"C-KF-10"
resultadoNy$metodo[6]<-"C-kF-60"
resultadoNy$metodo[7]<-"C-kF-120"
resultadoNy$metodo[8]<-"C-KF-ALL"
resultadoNy$metodo[9]<-"C-KFF-10"
resultadoNy$metodo[10]<-"C-KFF-60"
resultadoNy$metodo[11]<-"C-KFF-120"
resultadoNy$metodo[12]<-"C-KFF-ALL"
resultadoNy$metodo[13]<-"S-T-06"
resultadoNy$metodo[14]<-"S-T-07"
resultadoNy$metodo[15]<-"S-T-08"
resultadoNy$metodo[16]<-"S-T-09"

resultadoNy$enfoque[1]<-"clasico"
resultadoNy$enfoque[2]<-"clasico"
resultadoNy$enfoque[3]<-"clasico"
resultadoNy$enfoque[4]<-"clasico"
resultadoNy$enfoque[5]<-"propuesto"
resultadoNy$enfoque[6]<-"propuesto"
resultadoNy$enfoque[7]<-"propuesto"
resultadoNy$enfoque[8]<-"propuesto"
resultadoNy$enfoque[9]<-"propuesto"
resultadoNy$enfoque[10]<-"propuesto"
resultadoNy$enfoque[11]<-"propuesto"
resultadoNy$enfoque[12]<-"propuesto"
resultadoNy$enfoque[13]<-"clasico"
resultadoNy$enfoque[14]<-"clasico"
resultadoNy$enfoque[15]<-"clasico"
resultadoNy$enfoque[16]<-"clasico"

resultadoNy$metodo<-as.factor(resultadoNy$metodo)
resultadoNy$metodo <- factor(resultadoNy$metodo, levels = resultadoNy$metodo[order(resultadoNy$Mae)])

png(filename="informe-jaiio-2015/figuras/ny_resultado_ubcf.png",width = 400 , height=400)
ggplot(data=resultadoNy, aes(x=metodo,y=Mae,fill=enfoque))+ 
    geom_bar(stat="identity",position=position_dodge())+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
###############################
##varibles<-c("Agregation","Nvecinos","Mae","Rms","Precision","Recall","F_measure")
varibles<-c("Agregation","Nvecinos","Precision","Recall","F_measure")
##Neighbor Weighting
resultadosL<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCF_NY_UE_NeighborWeightingOverlapLiked.csv")
resultadosLH<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCF_NY_UE_NeighborWeightingOverlapLikedAndHated.csv")
resultadosL$F_measure<-(2*(resultadosL$Precision*resultadosL$Recall))/(resultadosL$Precision+resultadosL$Recall)
resultadosLH$F_measure<-(2*(resultadosLH$Precision*resultadosLH$Recall))/(resultadosLH$Precision+resultadosLH$Recall)

##OverlapLiked
resultadosL[,varibles]
melt_resultadosL<-melt(resultadosL[,varibles], id.vars = c("Agregation","Nvecinos"))
setEPS()
postscript("informe-experimento-likehated/figuras/neighbor_weighting_overlapliked.eps")
#pdf("informe-experimento-likehated/figuras/neighbor_weighting_overlapliked.pdf")
ggplot(melt_resultadosL, aes(x=Nvecinos, y=value)) +
    geom_line(aes(linetype=Agregation), # Line type depends on cond
               size = 1) +       # Thicker line
    geom_point(aes(shape=Agregation),
               fill = "white", # Shape depends on cond
               size = 4)   +   
            facet_grid(. ~variable) + 
            scale_x_continuous(breaks=c(10,60,120,200))+  
            xlab("Nro. Vecinos") +  
            ylab("Valor") +  
            theme(legend.title=element_blank()) + 
            theme(legend.position="bottom") + 
            scale_shape_discrete(name  ="name", breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score"))+
            scale_linetype_manual(values=c("solid","dotdash", "dotted"),name  ="name",breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score")) 
dev.off()
##OverlapLikedAndHated 
resultadosLH[,varibles]
melt_resultadosLH<-melt(resultadosLH[,varibles], id.vars = c("Agregation","Nvecinos"))
setEPS()
postscript("informe-experimento-likehated/figuras/neighbor_weighting_overlaplikedhated.eps")
ggplot(melt_resultadosLH, aes(x=Nvecinos, y=value)) +
    geom_line(aes(linetype=Agregation), # Line type depends on cond
              size = 1) +       # Thicker line
    geom_point(aes(shape=Agregation),
               fill = "white", # Shape depends on cond
               size = 4)   +  
    facet_grid(. ~variable) + 
    scale_x_continuous(breaks=c(10,60,120,200))+  
    xlab("Nro. Vecinos") +  
    ylab("Valor") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom") + 
    scale_shape_discrete(name  ="name", breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score"))+
    scale_linetype_manual(values=c("solid","dotdash", "dotted"),name  ="name",breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score")) 
dev.off()
##Neighbor Selection
resultadosSL<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCF_NY_UE_NeighborSelectionOverlapLiked.csv")
resultadosSLH<-read.csv("C:/Users/Usuarioç/Desktop/carlos/Tesis/datasets/foursquare/resultados/resultadosUBCF_NY_UE_NeighborSelectionOverlapLikedAndHated.csv")
resultadosSL$F_measure<-(2*(resultadosSL$Precision*resultadosSL$Recall))/(resultadosSL$Precision+resultadosSL$Recall)
resultadosSLH$F_measure<-(2*(resultadosSLH$Precision*resultadosSLH$Recall))/(resultadosSLH$Precision+resultadosSLH$Recall)

##OverlapLiked
resultadosSL[,varibles]
melt_resultadosSL<-melt(resultadosSL[,varibles], id.vars = c("Agregation","Nvecinos"))
setEPS()
postscript("informe-experimento-likehated/figuras/neighbor_selection_overlapliked.eps")
ggplot(melt_resultadosSL, aes(x=Nvecinos, y=value)) +
    geom_line(aes(linetype=Agregation), # Line type depends on cond
              size = 1) +       # Thicker line
    geom_point(aes(shape=Agregation),
               fill = "white", # Shape depends on cond
               size = 4)   +   
    facet_grid(. ~variable) + 
    scale_x_continuous(breaks=c(10,60,120,200))+  
    xlab("Nro. Vecinos") +  
    ylab("Valor") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom") + 
    scale_shape_discrete(name  ="name", breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score"))+
    scale_linetype_manual(values=c("solid","dotdash", "dotted"),name  ="name",breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score")) 
dev.off()
##OverlapLikedAndHated 
resultadosSLH[,varibles]
melt_resultadosSLH<-melt(resultadosSLH[,varibles], id.vars = c("Agregation","Nvecinos"))
setEPS()
postscript("informe-experimento-likehated/figuras/neighbor_selection_overlaplikedhated.eps")
ggplot(melt_resultadosSLH, aes(x=Nvecinos, y=value)) +
    geom_line(aes(linetype=Agregation), # Line type depends on cond
              size = 1) +       # Thicker line
    geom_point(aes(shape=Agregation),
               fill = "white", # Shape depends on cond
               size = 4)   +  
    facet_grid(. ~variable) + 
    scale_x_continuous(breaks=c(10,60,120,200))+  
    xlab("Nro. Vecinos") +  
    ylab("Valor") +  
    theme(legend.title=element_blank()) + 
    theme(legend.position="bottom") + 
    scale_shape_discrete(name  ="name", breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score"))+
    scale_linetype_manual(values=c("solid","dotdash", "dotted"),name  ="name",breaks=c("BASE", "HARMONIC_MEAN","ONLY_SCORE"),labels=c("Base", "Harmonic Mean","Only Score")) 
dev.off()

