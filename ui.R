library(markdown)

shinyUI(navbarPage("EMN Worldwide",
  tabPanel("Interest 2019-2020",
    sidebarLayout(
      sidebarPanel(
        selectInput("EMN", "Select a Multinational:", 
                    choices = c("Airbus SE", "Amazon.com Inc", "Apple Inc", "Banco Santander SA", "Enel SpA", "Facebook Inc", "HP Inc", "Heineken NV", "Ford Motor Co", "Walmart Inc")
        )
      ),
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  tabPanel("Taxes",
    sidebarLayout(
      sidebarPanel(
        selectInput("Plot", "Select a Plot:", 
                    choices = c("Mapa Mundi Plot", "Bar Plot")
               )
             ),
             mainPanel(
               plotOutput("TaxPlot")
             )
           )
  )
  

))

