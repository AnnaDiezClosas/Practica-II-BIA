#proyecto



dfIndicators <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=".", quote = "\"'",
                      header=TRUE, skip = 1, na.strings = "NA")
str(dfIndicators)
dfIndicators1 <- dfIndicators
dfIndicators1 <- dfIndicators1[,-5]
dfIndicators
dfIndicators1

#funciona?
