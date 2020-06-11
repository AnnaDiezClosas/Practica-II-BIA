#proyecto

if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
if(!require("ggthemes")) {
  install.packages("ggthemes")
  library("ggthemes")
}


if(!require("base")) {
  install.packages("base")
  library("base")
}
if(!require("scales")) {
  install.packages("scales")
  library("scales")
}

if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}
if(!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if(!require("rgdal")) {
  install.packages("rgdal")
  library("rgdal")
}

if(!require("sp")) {
  install.packages("sp")
  library("sp")
}

if(!require("maps")) {
  install.packages("maps")
  library("maps")
}

if(!require("rgeos")) {
  install.packages("rgeos")
  library("rgeos")
}

if(!require("gpclib")) {
  install.packages("gpclib")
  library("gpclib")
}

if(!require("maptools")) {
  install.packages("maptools")
  library("maptools")
}


####¿Dónde se encuentran las sedes principales de las EMN?####

dfIndicators<- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",", quote = "\"'",
                          header=TRUE, skip = 0, na.strings = "NA")

ggplot(dfIndicators, aes(x=Headquarters.of.Parent.MNE)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle ("¿Dónde se encuentran las sedes principales de las EMN?") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1.5, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) +
  xlab("País")+ylab("Número de empresas")

####¿Cuáles son los 35 países donde hay más EMN que pagan impuestos?####

# Se lee la base de datos
dfImpuestos <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",",
                          header=FALSE)

# Se eliminan todas las columnas menos las de interés (Nombre de la empresa, Paises donde puede estar presente la empresa)
dfImpuestos1 <- dfImpuestos[,-(2:11)]

#Se convierten a caracter todas las variables del data frame
dfImpuestos1[,-1]<-apply(dfImpuestos1[,-1],2,as.character)

#Se transpone el dataframe y se le añade a la columna de País el título
dfImpuestos1_transpose <- data.frame(t(dfImpuestos1[-1]))
names(dfImpuestos1_transpose)[1]="País"

#Se añaden tres columnas necesarias que corresponden a al sumatorio de cada tipo de presencia de las distintas empresas
dfImpuestos1_transpose$TotalAnnualReport=rowSums(dfImpuestos1_transpose[-1]=="Annual Reporting")
dfImpuestos1_transpose$TotalPhysical=rowSums(dfImpuestos1_transpose[-1]=="Physical")
dfImpuestos1_transpose$PresenciaTotalFisica=(dfImpuestos1_transpose$TotalAnnualReport+dfImpuestos1_transpose$TotalPhysical)

#Se ordena el dataframe de mayor a menor segun la presencia total de EMN en el País y se segmenta para coger los 35 con mas presencia y las columnas de País, presencia fisica del anual report y presencia fisica del registro fisico
dfImpuestos1_transpose<- arrange(dfImpuestos1_transpose, desc(PresenciaTotalFisica))
dfImpuestos_transposeSimplificada <- dfImpuestos1_transpose[1:35,c(1,502,503)]

#Se transforma el dataframe para poder hacer le grafico que se desea de tal manera que se obtiene una columna con pais una columna con la variable y otra con la frecuencia de cada opcion para cada pais
dfImpuestos_transposeSimplificada1<-gather(dfImpuestos_transposeSimplificada,"variable","Frequency",-1)

#Se grafica la información de interes
ggplot(dfImpuestos_transposeSimplificada1)+geom_bar(aes(x=País,y=Frequency,fill=variable),stat='identity') + scale_fill_grey() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ ggtitle ("35 países donde hay más EMN que pagan impuestos") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) +xlab("País")+ylab("Número de empresas")

####¿Cuáles son los 35 países donde se encuentran más EMN y en que registro, físico o digital?####

#Se añade una columna con el recuento de las empresas con presencia digital en cada País
dfImpuestos1_transpose$TotalDigital=rowSums(dfImpuestos1_transpose[-1]=="Digital")

#Se odena el dataframe de mayor  amenor presencia digital y se eliminan todas las filas menos las 35 con mayor presencia digital y las columnas de país, presencia fisica total y presencia digital
dfImpuestos1_transpose<- arrange(dfImpuestos1_transpose, desc(PresenciaTotalFisica))
dfImpuestos_transposeFormato <- dfImpuestos1_transpose[1:35,c(1,504,505)]

#Se transforma el dataframe para poder hacer le grafico que se desea de tal manera que se obtiene una columna con pais una columna con la variable y otra con la frecuencia de cada opcion para cada pais
dfImpuestos_transposeFormato1<-gather(dfImpuestos_transposeFormato,"variable","Frequency",-1)

#Se grafica la información de interes
ggplot(dfImpuestos_transposeFormato1)+geom_bar(aes(x=País,y=Frequency,fill=variable),stat='identity') + scale_fill_grey() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ ggtitle ("35 países con más presencia fisica y digital de EMN") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5))+ xlab("País")+ylab("Número de empresas")


#¿Qué países tienen más presencia física de EMN?
#EN UN MAPAMUNDI:

world.map <- readOGR(dsn="C:/Users/adiez/Desktop/Business Intelligence/Practica-II-BIA",layer="TM_WORLD_BORDERS-0.3") #SE TIENE QUE PONER CARPETA DONDE ESTÁ GUARDADO EL ARCHIVO CAMBIANDO \ POR /
world.ggmap <- fortify(world.map, region = "ISO2")
head(world.map@data)

tcp<-dfImpuestos1_transpose[,c(1,502)]

names(tcp)<-c("id","PresenciaTotalFisica")
tcp$id<-tolower(tcp$id)
world.ggmap$id<-tolower(world.ggmap$id)

world.ggmape <- merge(world.ggmap, tcp, by = "id", all = TRUE)
world.ggmape <- world.ggmape[order(world.ggmape$order), ]

world.plot <- ggplot(data = world.ggmape, aes(x = long, y = lat, group = group))+ geom_polygon(aes(fill =PresenciaTotalFisica), stat='identity')+ geom_path(aes(x=long, y=lat, group=group), color="gray")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ ggtitle ("¿Qué países tienen más presencia física de EMN?") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
world.plot

#¿Qué países tienen más presencia digital de EMN? 
tcp2<-dfImpuestos1_transpose[,c(1,503)]
names(tcp2)<-c("id","TotalDigital")
tcp2$id<-tolower(tcp2$id)
world.ggmap$id<-tolower(world.ggmap$id)

world.ggmape2 <- merge(world.ggmap, tcp2, by = "id", all = TRUE)
world.ggmape2 <- world.ggmape2[order(world.ggmape2$order), ]

world.plot2 <- ggplot(data = world.ggmape2, aes(x = long, y = lat, group = group))+ geom_polygon(aes(fill =TotalDigital), stat='identity')+ geom_path(aes(x=long, y=lat, group=group), color="gray")+ ggtitle ("¿Qué países tienen más presencia digital de EMN?") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

world.plot2



#¿Cómo ha afectado el COVID-19 a 10 de las EMN?

dfTrends <- read.table(file = "OECD-ADIMA-500-Google-trends-monitor.txt", header = FALSE, sep = "\t", dec =",")
dfTrends$V1 <- as.character(dfTrends$V1)

names(dfTrends)[2:76] <- format(as.Date(names(dfTrends)[2:76], format = "X%d.%m.%Y"), format = "%Y-%m-%d")

dfTrends1 <- dfTrends[c(1,17,26,39,57,168,178,202,205,187,441),] #ahora todas filas y solo columnas de las empresas que queremos analizar

dfTrends1_transpose <- data.frame(t(dfTrends1[-1]))
colnames(dfTrends1_transpose) <- dfTrends1[, 1]

names (dfTrends1_transpose)[1] = "Date"
rownames(dfTrends1_transpose)<-1:75

dfTrends1_transpose$`Airbus SE` <- as.numeric(sub(",",".",dfTrends1_transpose$`Airbus SE`,fixed=TRUE))
dfTrends1_transpose$`Amazon.com Inc` <- as.numeric(sub(",",".",dfTrends1_transpose$`Amazon.com Inc`,fixed=TRUE))
dfTrends1_transpose$`Apple Inc` <- as.numeric(sub(",",".",dfTrends1_transpose$`Apple Inc`,fixed=TRUE))
dfTrends1_transpose$`Banco Santander SA` <- as.numeric(sub(",",".",dfTrends1_transpose$`Banco Santander SA`,fixed=TRUE))
dfTrends1_transpose$`Enel SpA` <- as.numeric(sub(",",".",dfTrends1_transpose$`Enel SpA`,fixed=TRUE))
dfTrends1_transpose$`Facebook Inc` <- as.numeric(sub(",",".",dfTrends1_transpose$`Facebook Inc`,fixed=TRUE))
dfTrends1_transpose$`HP Inc` <- as.numeric(sub(",",".",dfTrends1_transpose$`HP Inc`,fixed=TRUE))
dfTrends1_transpose$`Heineken NV` <- as.numeric(sub(",",".",dfTrends1_transpose$`Heineken NV`,fixed=TRUE))
dfTrends1_transpose$`Ford Motor Co` <- as.numeric(sub(",",".",dfTrends1_transpose$`Ford Motor Co`,fixed=TRUE))
dfTrends1_transpose$`Walmart Inc` <- as.numeric(sub(",",".",dfTrends1_transpose$`Walmart Inc`,fixed=TRUE))


dfTrends1_transpose$Date <-as.Date(dfTrends1_transpose$Date, format = "%d/%m/%Y")

f1<-ggplot(dfTrends1_transpose, aes(Date, `Airbus SE` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Airbus")
f1
f2<-ggplot(dfTrends1_transpose, aes(Date, `Amazon.com Inc` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Amazon")
f2
f3<-ggplot(dfTrends1_transpose, aes(Date, `Apple Inc` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Apple")
f3
f4<-ggplot(dfTrends1_transpose, aes(Date, `Banco Santander SA` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Banco Santander")
f4
f5<-ggplot(dfTrends1_transpose, aes(Date, `Enel SpA` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Enel")
f5
f6<-ggplot(dfTrends1_transpose, aes(Date, `Facebook Inc` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Facebook")
f6
f7<-ggplot(dfTrends1_transpose, aes(Date, `HP Inc` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index HP")
f7
f8<-ggplot(dfTrends1_transpose, aes(Date, `Heineken NV` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Heineken")
f8
f9<-ggplot(dfTrends1_transpose, aes(Date, `Ford Motor Co` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Ford")
f9
f10<-ggplot(dfTrends1_transpose, aes(Date, `Walmart Inc` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+labs(x="Date", y="Index Walmart")
f10

ftot<-ggplot(dfTrends1_transpose, aes(Date, `Airbus SE` )) + scale_colour_identity()+geom_line(color="blue")+
  theme_minimal()+geom_line(aes(Date, `Amazon.com Inc`),color="red")+
  geom_line(aes(Date, `Apple Inc`),color="green")+
  geom_line(aes(Date, `Banco Santander SA`),color="orange")+
  geom_line(aes(Date, `Enel SpA`),color="grey")+labs(x="Date", y="Index")
ftot #no se entiende nada

pal <- colorRampPalette(c("#488f31","#9dc6e0","#a5c796","#f89a5f","#de425b"))

TOT1<-ggplot(data=dfTrends1_transpose,aes(x=Date))+geom_line(aes(y=`Airbus SE`,colour="Airbus SE"))+
  geom_line(aes(y=`Amazon.com Inc`,colour="Amazon.com Inc"))+
  geom_line(aes(y=`Apple Inc`,colour="Apple Inc"))+
  geom_line(aes(y=`Banco Santander SA`,colour="Banco Santander SA"))+
  geom_line(aes(y=`Enel SpA`,colour="Enel SpA"))+
    scale_colour_manual("", breaks=c("Airbus SE","Amazon.com Inc", "Apple Inc","Banco Santander SA","Enel SpA"), values=c("red","blue","green","orange","grey"))+
  xlab("Date")+ylab("Index") + ggtitle ("Tendencía del interes de las EMN") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(2), 
                                   vjust=2, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
TOT1

TOT2<-ggplot(data=dfTrends1_transpose,aes(x=Date))+
    geom_line(aes(y=`Facebook Inc`,colour="Facebook Inc"))+
  geom_line(aes(y=`HP Inc`,colour="HP Inc"))+
  geom_line(aes(y=`Heineken NV`,colour="Heineken NV"))+
  geom_line(aes(y=`Ford Motor Co`,colour="Ford Motor Co"))+
  geom_line(aes(y=`Walmart Inc`,colour="Walmart Inc"))+
  scale_colour_manual("", breaks=c("Facebook Inc","HP Inc","Heineken NV","Ford Motor Co","Walmart Inc"), values=c("red","blue","green","orange","grey"))+
  xlab("Date")+ylab("Index")+ ggtitle ("Tendencía del interes de las EMN") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(2), 
                                   vjust=2, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5))
TOT2

#¿Cómo difiere la tendencia de interés 2019 vs 2020 de estas 10 EMN?
#AIRBUS, 2019 vs. 2020

dfAirbus <- dfTrends1_transpose[,c("Date","Airbus SE")]
names (dfAirbus)[2] = "Index"
dfAirbus<-dfAirbus[-(1:5),]

dfAirbus2 <- dfAirbus

dfAirbus<-dfAirbus[1:18,]
dfAirbus2<-dfAirbus2[53:70,]

dfAirbus$'Index 2020' <- dfAirbus2$Index
names (dfAirbus)[2] = "Index 2019"

AIRBUS<-ggplot(data=dfAirbus,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("AIRBUS")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
AIRBUS

#AMAZON, 2019 vs. 2020
dfAmazon <- dfTrends1_transpose[,c("Date","Amazon.com Inc")]
names (dfAmazon)[2] = "Index"
dfAmazon<-dfAmazon[-(1:5),]

dfAmazon2 <- dfAmazon

dfAmazon<-dfAmazon[1:18,]
dfAmazon2<-dfAmazon2[53:70,]

dfAmazon$'Index 2020' <- dfAmazon2$Index
names (dfAmazon)[2] = "Index 2019"


AMAZON<-ggplot(data=dfAmazon,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("AMAZON")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
AMAZON

#APPLE, 2019 vs. 2020
dfApple <- dfTrends1_transpose[,c("Date","Apple Inc")]
names (dfApple)[2] = "Index"
dfApple<-dfApple[-(1:5),]

dfApple2 <- dfApple

dfApple<-dfApple[1:18,]
dfApple2<-dfApple2[53:70,]

dfApple$'Index 2020' <- dfApple2$Index
names (dfApple)[2] = "Index 2019"

APPLE<-ggplot(data=dfApple,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("APPLE")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
APPLE

#BANCO SANTANDER, 2019 vs. 2020
dfSantander <- dfTrends1_transpose[,c("Date","Banco Santander SA")]
names (dfSantander)[2] = "Index"
dfSantander<-dfSantander[-(1:5),]

dfSantander2 <- dfSantander

dfSantander<-dfSantander[1:18,]
dfSantander2<-dfSantander2[53:70,]

dfSantander$'Index 2020' <- dfSantander2$Index
names (dfSantander)[2] = "Index 2019"

SANTANDER<-ggplot(data=dfSantander,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("BANCO SANTANDER")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
SANTANDER

#ENEL, 2019 vs. 2020
dfEnel <- dfTrends1_transpose[,c("Date","Enel SpA")]
names (dfEnel)[2] = "Index"
dfEnel<-dfEnel[-(1:5),]

dfEnel2 <- dfEnel

dfEnel<-dfEnel[1:18,]
dfEnel2<-dfEnel2[53:70,]

dfEnel$'Index 2020' <- dfEnel2$Index
names (dfEnel)[2] = "Index 2019"

ENEL<-ggplot(data=dfEnel,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("ENEL")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
ENEL

#FACEBOOK, 2019 vs. 2020
dfFacebook <- dfTrends1_transpose[,c("Date","Facebook Inc")]
names (dfFacebook)[2] = "Index"
dfFacebook<-dfFacebook[-(1:5),]

dfFacebook2 <- dfFacebook

dfFacebook<-dfFacebook[1:18,]
dfFacebook2<-dfFacebook2[53:70,]

dfFacebook$'Index 2020' <- dfFacebook2$Index
names (dfFacebook)[2] = "Index 2019"

FACEBOOK<-ggplot(data=dfFacebook,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("FACEBOOK")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
FACEBOOK

#HP, 2019 vs. 2020
dfHP <- dfTrends1_transpose[,c("Date","HP Inc")]
names (dfHP)[2] = "Index"
dfHP<-dfHP[-(1:5),]

dfHP2 <- dfHP

dfHP<-dfHP[1:18,]
dfHP2<-dfHP2[53:70,]

dfHP$'Index 2020' <- dfHP2$Index
names (dfHP)[2] = "Index 2019"

HP<-ggplot(data=dfHP,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("HP")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
HP

#HEINEKEN, 2019 vs. 2020
dfHeineken <- dfTrends1_transpose[,c("Date","Heineken NV")]
names (dfHeineken)[2] = "Index"
dfHeineken<-dfHeineken[-(1:5),]

dfHeineken2 <- dfHeineken

dfHeineken<-dfHeineken[1:18,]
dfHeineken2<-dfHeineken2[53:70,]

dfHeineken$'Index 2020' <- dfHeineken2$Index
names (dfHeineken)[2] = "Index 2019"

HEINEKEN<-ggplot(data=dfHeineken,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("HEINEKEN")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
HEINEKEN

#FORD, 2019 vs. 2020
dfFord <- dfTrends1_transpose[,c("Date","Ford Motor Co")]
names (dfFord)[2] = "Index"
dfFord<-dfFord[-(1:5),]

dfFord2 <- dfFord

dfFord<-dfFord[1:18,]
dfFord2<-dfFord2[53:70,]

dfFord$'Index 2020' <- dfFord2$Index
names (dfFord)[2] = "Index 2019"

FORD<-ggplot(data=dfFord,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("FORD")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
FORD

#WALMART, 2019 vs. 2020
dfWalmart <- dfTrends1_transpose[,c("Date","Walmart Inc")]
names (dfWalmart)[2] = "Index"
dfWalmart<-dfWalmart[-(1:5),]

dfWalmart2 <- dfWalmart

dfWalmart<-dfWalmart[1:18,]
dfWalmart2<-dfWalmart2[53:70,]

dfWalmart$'Index 2020' <- dfWalmart2$Index
names (dfWalmart)[2] = "Index 2019"

WALMART<-ggplot(data=dfWalmart,aes(x=Date))+
  geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
  geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
  scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
  ylab("Index")+ggtitle ("WALMART")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 
WALMART

#¿Qué temas o sectores preocupan o interesan más dentro de cada una de estas 10 EMN?

dfIndex <- read.table("OECD-ADIMA-500-IndexConstituyents.txt", sep="\t", dec=",", quote = "\"'",
                           header=TRUE, skip = 0, na.strings = "NA")

dfIndex <- dfIndex[,c(1,4,8)]

#DENTRO DE AIRBUS

dfIndexAirbus<- dfIndex[dfIndex$Parent.MNE=="Airbus SE",]

dfIndexAirbus$percent <- floor(dfIndexAirbus$Weight)
dfIndexAirbus <- dfIndexAirbus[order(desc(dfIndexAirbus$Weight-dfIndexAirbus$percent)),]
dfIndexAirbus$percent <- dfIndexAirbus$percent + ifelse(1:nrow(dfIndexAirbus)>100-sum(dfIndexAirbus$percent),0,1) 
dfIndexAirbus <- dfIndexAirbus[order(desc(dfIndexAirbus$percent)),]

df <- expand.grid(x=1:10,y=1:10)
df$WikiTopic <- rep(dfIndexAirbus$WikiTopic, dfIndexAirbus$percent)

ggplot(df,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("AIRBUS")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO DE AMAZON

dfIndexAmazon<- dfIndex[dfIndex$Parent.MNE=="Amazon.com Inc",]

dfIndexAmazon$percent <- floor(dfIndexAmazon$Weight)
dfIndexAmazon <- dfIndexAmazon[order(desc(dfIndexAmazon$Weight-dfIndexAmazon$percent)),]
dfIndexAmazon$percent <- dfIndexAmazon$percent + ifelse(1:nrow(dfIndexAmazon)>100-sum(dfIndexAmazon$percent),0,1) 
dfIndexAmazon <- dfIndexAmazon[order(desc(dfIndexAmazon$percent)),]

df1 <- expand.grid(x=1:10,y=1:10)
df1$WikiTopic <- rep(dfIndexAmazon$WikiTopic, dfIndexAmazon$percent)

ggplot(df1,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("AMAZON")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5))  

#DENTRO DE APPLE

dfIndexApple<- dfIndex[dfIndex$Parent.MNE=="Apple Inc",]

dfIndexApple$percent <- floor(dfIndexApple$Weight)
dfIndexApple <- dfIndexApple[order(desc(dfIndexApple$Weight-dfIndexApple$percent)),]
dfIndexApple$percent <- dfIndexApple$percent + ifelse(1:nrow(dfIndexApple)>100-sum(dfIndexApple$percent),0,1) 
dfIndexApple <- dfIndexApple[order(desc(dfIndexApple$percent)),]

df2 <- expand.grid(x=1:10,y=1:10)
df2$WikiTopic <- rep(dfIndexApple$WikiTopic, dfIndexApple$percent)

ggplot(df2,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("APPLE")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO DE BANCO SANTANDER

dfIndexSantander<- dfIndex[dfIndex$Parent.MNE=="Banco Santander SA",]

dfIndexSantander$percent <- floor(dfIndexSantander$Weight)
dfIndexSantander <- dfIndexSantander[order(desc(dfIndexSantander$Weight-dfIndexSantander$percent)),]
dfIndexSantander$percent <- dfIndexSantander$percent + ifelse(1:nrow(dfIndexSantander)>100-sum(dfIndexSantander$percent),0,1) 
dfIndexSantander <- dfIndexSantander[order(desc(dfIndexSantander$percent)),]

df3 <- expand.grid(x=1:10,y=1:10)
df3$WikiTopic <- rep(dfIndexSantander$WikiTopic, dfIndexSantander$percent)

ggplot(df3,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("BANCO SANTANDER")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO ENEL

dfIndexEnel<- dfIndex[dfIndex$Parent.MNE=="Enel SpA",]

dfIndexEnel$percent <- floor(dfIndexEnel$Weight)
dfIndexEnel <- dfIndexEnel[order(desc(dfIndexEnel$Weight-dfIndexEnel$percent)),]
dfIndexEnel$percent <- dfIndexEnel$percent + ifelse(1:nrow(dfIndexEnel)>100-sum(dfIndexEnel$percent),0,1) 
dfIndexEnel <- dfIndexEnel[order(desc(dfIndexEnel$percent)),]

df4 <- expand.grid(x=1:10,y=1:10)
df4$WikiTopic <- rep(dfIndexEnel$WikiTopic, dfIndexEnel$percent)

ggplot(df4,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("ENEL")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO FACEBOOK

dfIndexFacebook<- dfIndex[dfIndex$Parent.MNE=="Facebook Inc",]

dfIndexFacebook$percent <- floor(dfIndexFacebook$Weight)
dfIndexFacebook <- dfIndexFacebook[order(desc(dfIndexFacebook$Weight-dfIndexFacebook$percent)),]
dfIndexFacebook$percent <- dfIndexFacebook$percent + ifelse(1:nrow(dfIndexFacebook)>100-sum(dfIndexFacebook$percent),0,1) 
dfIndexFacebook <- dfIndexFacebook[order(desc(dfIndexFacebook$percent)),]

df5 <- expand.grid(x=1:10,y=1:10)
df5$WikiTopic <- rep(dfIndexFacebook$WikiTopic, dfIndexFacebook$percent)

ggplot(df5,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("FACEBOOK")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5))

#DENTRO HP

dfIndexHP<- dfIndex[dfIndex$Parent.MNE=="HP Inc",]

dfIndexHP$percent <- floor(dfIndexHP$Weight)
dfIndexHP <- dfIndexHP[order(desc(dfIndexHP$Weight-dfIndexHP$percent)),]
dfIndexHP$percent <- dfIndexHP$percent + ifelse(1:nrow(dfIndexHP)>100-sum(dfIndexHP$percent),0,1) 
dfIndexHP <- dfIndexHP[order(desc(dfIndexHP$percent)),]

df6 <- expand.grid(x=1:10,y=1:10)
df6$WikiTopic <- rep(dfIndexHP$WikiTopic, dfIndexHP$percent)

ggplot(df6,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("HP")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO HEINEKEN

dfIndexHeineken<- dfIndex[dfIndex$Parent.MNE=="Heineken NV",]

dfIndexHeineken$percent <- floor(dfIndexHeineken$Weight)
dfIndexHeineken <- dfIndexHeineken[order(desc(dfIndexHeineken$Weight-dfIndexHeineken$percent)),]
dfIndexHeineken$percent <- dfIndexHeineken$percent + ifelse(1:nrow(dfIndexHeineken)>100-sum(dfIndexHeineken$percent),0,1) 
dfIndexHeineken <- dfIndexHeineken[order(desc(dfIndexHeineken$percent)),]

df7 <- expand.grid(x=1:10,y=1:10)
df7$WikiTopic <- rep(dfIndexHeineken$WikiTopic, dfIndexHeineken$percent)

ggplot(df7,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("HEINEKEN")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 

#DENTRO FORD

dfIndexFord<- dfIndex[dfIndex$Parent.MNE=="Ford Motor Co",]

dfIndexFord$percent <- floor(dfIndexFord$Weight)
dfIndexFord <- dfIndexFord[order(desc(dfIndexFord$Weight-dfIndexFord$percent)),]
dfIndexFord$percent <- dfIndexFord$percent + ifelse(1:nrow(dfIndexFord)>100-sum(dfIndexFord$percent),0,1) 
dfIndexFord <- dfIndexFord[order(desc(dfIndexFord$percent)),]

df8 <- expand.grid(x=1:10,y=1:10)
df8$WikiTopic <- rep(dfIndexFord$WikiTopic, dfIndexFord$percent)

ggplot(df8,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("FORD")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5))

#DENTRO WALMART

dfIndexWalmart<- dfIndex[dfIndex$Parent.MNE=="Walmart Inc",]

dfIndexWalmart$percent <- floor(dfIndexWalmart$Weight)
dfIndexWalmart <- dfIndexWalmart[order(desc(dfIndexWalmart$Weight-dfIndexWalmart$percent)),]
dfIndexWalmart$percent <- dfIndexWalmart$percent + ifelse(1:nrow(dfIndexWalmart)>100-sum(dfIndexWalmart$percent),0,1) 
dfIndexWalmart <- dfIndexWalmart[order(desc(dfIndexWalmart$percent)),]

df9 <- expand.grid(x=1:10,y=1:10)
df9$WikiTopic <- rep(dfIndexWalmart$WikiTopic, dfIndexWalmart$percent)

ggplot(df9,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+ggtitle ("WALMART")+  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) 


####¿Las empresas con un interés creciente tienen más presencia internacional?####

#Indice de internacionalidad de las empresas seleccionadas en GoogleTrends
#Se lee la base de datos
dfInternacionalidad<- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",", quote = "\"'",
                          header=TRUE, skip = 0, na.strings = "NA")

#Se seleccionan las filas correspondientes a las 10 EMN de estudio
dfInternacionalidad1 <- dfInternacionalidad[c(18,29,42,62,186,198,207,225,228,484),]
str(dfInternacionalidad1)

#Se grafica la información deseada
ggplot(data=dfInternacionalidad1, aes(x=Parent.MNE, y=International.Share, fill=Parent.MNE)) + 
  geom_bar(stat="identity", position="dodge") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ ggtitle ("Indice de internacionalidad") +  
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=1, 
                                   face="bold", 
                                   color="grey", 
                                   lineheight=1.5)) + xlab("MNE")+ylab("International Share")
