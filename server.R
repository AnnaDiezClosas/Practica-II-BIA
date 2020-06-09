library(shiny)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
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
      
      GRAFICO<-ggplot(data=dfAirbus,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    
    if (EMNselected=="Amazon.com Inc"){
      dfAmazon <- dfTrends1_transpose[,c("Date","Amazon.com Inc")]
      names (dfAmazon)[2] = "Index"
      dfAmazon<-dfAmazon[-(1:5),]
      
      dfAmazon2 <- dfAmazon
      
      dfAmazon<-dfAmazon[1:18,]
      dfAmazon2<-dfAmazon2[53:70,]
      
      dfAmazon$'Index 2020' <- dfAmazon2$Index
      names (dfAmazon)[2] = "Index 2019"
      
      
      GRAFICO<-ggplot(data=dfAmazon,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    
    if (EMNselected=="Apple Inc"){
      dfApple <- dfTrends1_transpose[,c("Date","Apple Inc")]
      names (dfApple)[2] = "Index"
      dfApple<-dfApple[-(1:5),]
      
      dfApple2 <- dfApple
      
      dfApple<-dfApple[1:18,]
      dfApple2<-dfApple2[53:70,]
      
      dfApple$'Index 2020' <- dfApple2$Index
      names (dfApple)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfApple,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    if (EMNselected=="Banco Santander SA"){
      dfSantander <- dfTrends1_transpose[,c("Date","Banco Santander SA")]
      names (dfSantander)[2] = "Index"
      dfSantander<-dfSantander[-(1:5),]
      
      dfSantander2 <- dfSantander
      
      dfSantander<-dfSantander[1:18,]
      dfSantander2<-dfSantander2[53:70,]
      
      dfSantander$'Index 2020' <- dfSantander2$Index
      names (dfSantander)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfSantander,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
      }
    if (EMNselected=="Enel SpA"){
      dfEnel <- dfTrends1_transpose[,c("Date","Enel SpA")]
      names (dfEnel)[2] = "Index"
      dfEnel<-dfEnel[-(1:5),]
      
      dfEnel2 <- dfEnel
      
      dfEnel<-dfEnel[1:18,]
      dfEnel2<-dfEnel2[53:70,]
      
      dfEnel$'Index 2020' <- dfEnel2$Index
      names (dfEnel)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfEnel,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    
    if (EMNselected=="Facebook Inc"){
      dfFacebook <- dfTrends1_transpose[,c("Date","Facebook Inc")]
      names (dfFacebook)[2] = "Index"
      dfFacebook<-dfFacebook[-(1:5),]
      
      dfFacebook2 <- dfFacebook
      
      dfFacebook<-dfFacebook[1:18,]
      dfFacebook2<-dfFacebook2[53:70,]
      
      dfFacebook$'Index 2020' <- dfFacebook2$Index
      names (dfFacebook)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfFacebook,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    if (EMNselected=="HP Inc"){
      dfHP <- dfTrends1_transpose[,c("Date","HP Inc")]
      names (dfHP)[2] = "Index"
      dfHP<-dfHP[-(1:5),]
      
      dfHP2 <- dfHP
      
      dfHP<-dfHP[1:18,]
      dfHP2<-dfHP2[53:70,]
      
      dfHP$'Index 2020' <- dfHP2$Index
      names (dfHP)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfHP,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    if (EMNselected=="Heineken NV"){
      dfHeineken <- dfTrends1_transpose[,c("Date","Heineken NV")]
      names (dfHeineken)[2] = "Index"
      dfHeineken<-dfHeineken[-(1:5),]
      
      dfHeineken2 <- dfHeineken
      
      dfHeineken<-dfHeineken[1:18,]
      dfHeineken2<-dfHeineken2[53:70,]
      
      dfHeineken$'Index 2020' <- dfHeineken2$Index
      names (dfHeineken)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfHeineken,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    if (EMNselected=="Ford Motor Co"){
      dfFord <- dfTrends1_transpose[,c("Date","Ford Motor Co")]
      names (dfFord)[2] = "Index"
      dfFord<-dfFord[-(1:5),]
      
      dfFord2 <- dfFord
      
      dfFord<-dfFord[1:18,]
      dfFord2<-dfFord2[53:70,]
      
      dfFord$'Index 2020' <- dfFord2$Index
      names (dfFord)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfFord,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    if (EMNselected=="Walmart Inc"){
      dfWalmart <- dfTrends1_transpose[,c("Date","Walmart Inc")]
      names (dfWalmart)[2] = "Index"
      dfWalmart<-dfWalmart[-(1:5),]
      
      dfWalmart2 <- dfWalmart
      
      dfWalmart<-dfWalmart[1:18,]
      dfWalmart2<-dfWalmart2[53:70,]
      
      dfWalmart$'Index 2020' <- dfWalmart2$Index
      names (dfWalmart)[2] = "Index 2019"
      
      GRAFICO<-ggplot(data=dfWalmart,aes(x=Date))+
        geom_line(aes(y=`Index 2019`,colour="Index 2019"))+
        geom_line(aes(y=`Index 2020`,colour="Index 2020"))+
        scale_colour_manual("", breaks=c("Index 2019","Index 2020"), values=c("red","blue"))+
        ylab("Index")
    }
    
    GRAFICO
    
  }) 

  output$TaxPlot <- renderPrint({
   
    Plotselected <- input$Plot
    
    if (Plotselected=="Bar Plot"){
      
      dfImpuestos <- read.table("OECD-ADIMA-Indicators.txt", sep="\t", dec=",",
                                header=FALSE)
      
      dfImpuestos1 <- dfImpuestos[,-(2:11)]
      
      dfImpuestos1[,-1]<-apply(dfImpuestos1[,-1],2,as.character)
      
      dfImpuestos1_transpose <- data.frame(t(dfImpuestos1[-1]))
      names(dfImpuestos1_transpose)[1]="País"
      
      dfImpuestos1_transpose$TotalAnnualReport=rowSums(dfImpuestos1_transpose[-1]=="Annual Reporting")
      dfImpuestos1_transpose$TotalPhysical=rowSums(dfImpuestos1_transpose[-1]=="Physical")
      
      dfImpuestos1_transpose$PresenciaTotalFisica=(dfImpuestos1_transpose$TotalAnnualReport+dfImpuestos1_transpose$TotalPhysical)
      
      dfImpuestos1_transpose<- arrange(dfImpuestos1_transpose, desc(PresenciaTotalFisica))
      
      dfImpuestos_transposeSimplificada <- dfImpuestos1_transpose[1:35,c(1,502,503)]
      
      
      dfImpuestos_transposeSimplificada1<-gather(dfImpuestos_transposeSimplificada,"variable","Frequency",-1)
      
      ggplot(dfImpuestos_transposeSimplificada1)+geom_bar(aes(x=País,y=Frequency,fill=variable),stat='identity') + scale_fill_grey()
    }
    
    
  })

  #output$table <- DT::renderDataTable({
   # DT::datatable(cars)
  #})
})
