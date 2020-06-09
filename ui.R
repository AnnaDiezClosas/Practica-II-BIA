#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(install.packages.check.source = "no")
pckgs<-c("tidyverse","shiny","ggthemes","RColorBrewer","carData")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Comparing online search interest between 2019 and 2020"),
  
  # Sidebar with a slider input for EMN
  sidebarPanel(
    selectInput("EMN", "Select a Multinational:", 
                choices = c("Airbus SE", "Amazon.com Inc", "Apple Inc", "Banco Santander SA", "Enel SpA", "Facebook Inc", "HP Inc", "Heineken NV", "Ford Motor Co", "Walmart Inc")),
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
)

