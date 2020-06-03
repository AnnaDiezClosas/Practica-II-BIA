#proyecto

dfIndicators <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=".", quote = "\"'",
                           header=TRUE, skip = 0, na.strings = "NA")

str(dfIndicators)
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

#¿Dónde están las empresas multinacionales?

ggplot(dfIndicators, aes(x=Headquarters.of.Parent.MNE)) + geom_bar()

#¿Dónde pagan impuestos?






#¿Cómo ha afectado COVID a las multinacionales?

dfTrends <- read.table("OECD-ADIMA-500-Google-trends-monitor.txt", sep="\t", dec=".", quote = "\"'",
                           header=TRUE, skip = 0, na.strings = "NA")

dfTrends1 <- dfTrends[c(16,25,440,38,437,404,56,177,201,204,167),] #ahora todas columnas y solo filas de las empresas seleccionadas
dfTest2