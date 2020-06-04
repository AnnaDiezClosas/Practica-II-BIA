#proyecto

dfIndicators <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",", quote = "\"'",
                           header=TRUE, skip = 0, na.strings = "NA")

str(dfIndicators)
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

#¿Dónde están las empresas multinacionales?

ggplot(dfIndicators, aes(x=Headquarters.of.Parent.MNE)) + geom_bar()

#¿Dónde pagan impuestos?
#Cambiar Not Found por NA




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
dfTrends1_transpose$Date <-as.Date(dfTrends1_transpose$Date, format = "%d/%m/%Y")

f1<-ggplot(dfTrends1_transpose, aes(Date, `Airbus SE` )) + scale_colour_identity()+geom_line(color="blue")+theme_minimal()
f1

       