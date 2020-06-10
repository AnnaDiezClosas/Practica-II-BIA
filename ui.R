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
        numericInput("bins",label= "Select the number of countries on which multinationals pay more taxes:", value=35, min=1)
             ),
             mainPanel(
               plotOutput("TaxPlot")
             )
           )
  )
  

))

