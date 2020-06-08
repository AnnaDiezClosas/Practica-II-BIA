<<<<<<< HEAD
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    EMNselected <- input$EMN
    
    # draw the line diagram
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
   
      if (EMNselected=="Airbus SE"){
      
      
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
    }

    
  }) 
  
})
=======
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    nbins <- input$bins
    coloresc <- input$radio
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(faithful,aes(x=waiting))+geom_histogram(bins = nbins, fill=coloresc ,col="darkgray")+
      theme_bw() + labs(title="Histograma del tiempo de espera") +
      theme(plot.title=element_text(size=24))
    
  }) 
  
})
>>>>>>> 609a09217fa7e6675d5a50fb265a8a0c632db485
