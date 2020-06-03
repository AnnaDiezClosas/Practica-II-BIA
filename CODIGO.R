#proyecto

dfIndicators <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=".", quote = "\"'",
                           header=TRUE, skip = 1, na.strings = "NA")

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
                           header=TRUE, skip = 1, na.strings = "NA")

dfTrends1 <- dfTrends[c(16,25,38,56,167,177,201,204,186),] #ahora todas filas y solo columnas 1,3
dfTest2