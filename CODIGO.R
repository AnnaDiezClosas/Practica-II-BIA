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

dfTrends <- read.table(file = "OECD-ADIMA-500-Google-trends-monitor.txt", header = TRUE, sep = "\t", dec =",")
dfTrends$Parent.MNE <- as.character(dfTrends$Parent.MNE)
names(dfTrends)[2:76] <- format(as.Date(names(dfTrends)[2:76], format = "X%d.%m.%Y"), format = "%Y-%m-%d")

dfTrends1 <- dfTrends[c(16,25,38,56,167,177,201,204,186,440),] #ahora todas filas y solo columnas de las empresas que queremos analizar
