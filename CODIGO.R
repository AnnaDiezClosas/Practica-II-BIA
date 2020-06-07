#proyecto

dfIndicators<- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",", quote = "\"'",
                           header=TRUE, skip = 0, na.strings = "NA")

str(dfIndicators)
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

#¿Dónde están las empresas multinacionales?

ggplot(dfIndicators, aes(x=Headquarters.of.Parent.MNE)) + geom_bar()

#¿Dónde pagan impuestos?#

dfImpuestos <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",",
                          header=FALSE)

dfImpuestos1 <- dfImpuestos[,-2:-3:-4:-5:-6:-7:-8:-9:-10:-11]

dfImpuestos1[,-1]<-apply(dfImpuestos1[,-1],2,as.character)

dfImpuestos1_transpose <- data.frame(t(dfImpuestos1[-1]))
names(dfImpuestos1_transpose)[1]="País"

dfImpuestos1_transpose$TotalAnnualReport=rowSums(dfImpuestos1_transpose[-1]=="Annual Reporting")
dfImpuestos1_transpose$TotalPhysical=rowSums(dfImpuestos1_transpose[-1]=="Physical")


ggplot(dfImpuestos1_transpose, aes(x=TotalAnnualReport)) + 
  geom_histogram(binwidth = 0.1, boundary=0, fill="grey",color="black")

#Impuestos según physical register

dfPhysical<- read.table("OECD-ADIMA-Physical-Register.txt", sep="\t", dec=",",
                          header=TRUE, skip = 0, na.strings = "NA")

ggplot(dfPhysical, aes(x=Jurisdiction)) + geom_bar() #No se ve muy bien hay demasiados paises..


# Ordeno alfabeticamente los paises y separo el dataframe
dfPhysical$Jurisdiction <- as.character(dfPhysical$Jurisdiction)
dfPhysical$Jurisdiction <- sort(dfPhysical$Jurisdiction, decreasing = FALSE)

#Hago dos graficos oara ver si se ve mejor...

dfPhysical1 <- dfPhysical[1:54452,]
dfPhysical2 <- dfPhysical[54453:116042,]
ggplot(dfPhysical1, aes(x=Jurisdiction)) + geom_bar() #sigue sin verse bien..
ggplot(dfPhysical2, aes(x=Jurisdiction)) + geom_bar() #sigue sin verse bien...

#Divido el grafico en más partes..

dfPhysical1 <- dfPhysical[1:20669,]
dfPhysical2 <- dfPhysical[20670:57696,]
dfPhysical3 <- dfPhysical[57697:74534,]
dfPhysical4 <- dfPhysical[74535:116042,]
ggplot(dfPhysical1, aes(x=Jurisdiction)) + geom_bar() 
ggplot(dfPhysical2, aes(x=Jurisdiction)) + geom_bar() 
ggplot(dfPhysical3, aes(x=Jurisdiction)) + geom_bar() 
ggplot(dfPhysical4, aes(x=Jurisdiction)) + geom_bar() 


#¿Cómo ha afectado COVID a las multinacionales?

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
  xlab("Date")+ylab("Index")
TOT1

TOT2<-ggplot(data=dfTrends1_transpose,aes(x=Date))+
    geom_line(aes(y=`Facebook Inc`,colour="Facebook Inc"))+
  geom_line(aes(y=`HP Inc`,colour="HP Inc"))+
  geom_line(aes(y=`Heineken NV`,colour="Heineken NV"))+
  geom_line(aes(y=`Ford Motor Co`,colour="Ford Motor Co"))+
  geom_line(aes(y=`Walmart Inc`,colour="Walmart Inc"))+
  scale_colour_manual("", breaks=c("Facebook Inc","HP Inc","Heineken NV","Ford Motor Co","Walmart Inc"), values=c("red","blue","green","orange","grey"))+
  xlab("Date")+ylab("Index")
TOT2

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
  ylab("Index")+ggtitle ("AIRBUS")
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
  ylab("Index")+ggtitle ("AMAZON")
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
  ylab("Index")+ggtitle ("APPLE")
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
  ylab("Index")+ggtitle ("BANCO SANTANDER")
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
  ylab("Index")+ggtitle ("ENEL")
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
  ylab("Index")+ggtitle ("FACEBOOK")
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
  ylab("Index")+ggtitle ("HP")
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
  ylab("Index")+ggtitle ("HEINEKEN")
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
  ylab("Index")+ggtitle ("FORD")
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
  ylab("Index")+ggtitle ("WALMART")
WALMART

#¿Qué temas o sectores preocupan más dentro de cada EMN? 

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
        axis.ticks = element_blank())

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
        axis.ticks = element_blank())

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
        axis.ticks = element_blank())

#DENTRO DE BANCO SANTANDER

dfIndexSantander<- dfIndex[dfIndex$Parent.MNE=="Banco Santander SA",]

dfIndexSantander$percent <- floor(dfIndexSantander$Weight)
dfIndexSantander <- dfIndexSantander[order(desc(dfIndexSantander$Weight-dfIndexSantander$percent)),]
dfIndexSantander$percent <- dfIndexSantander$percent + ifelse(1:nrow(dfIndexSantander)>100-sum(dfIndexSantander$percent),0,1) 
dfIndexApple <- dfIndexApple[order(desc(dfIndexApple$percent)),]

df3 <- expand.grid(x=1:10,y=1:10)
df3$WikiTopic <- rep(dfIndexSantander$WikiTopic, dfIndexSantander$percent)

ggplot(df3,aes(x=x,y=y,fill=WikiTopic))+
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  coord_equal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

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
        axis.ticks = element_blank())

