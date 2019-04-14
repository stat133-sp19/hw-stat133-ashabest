## Savings Simulator ##

library(shiny)
library(ggplot2)

ui <- fluidPage(
   
   titlePanel("Savings Simulator"),
   
   # widgets
   fluidRow(
     column(4,
            sliderInput("init",
                        "Initial Amount",
                        min = 1, max = 30, value = 20, step = 1),
            
            sliderInput("contrib",
                        "Annual Contribution",
                        min = 1, max = 30, value = 20, step = 1)
     ),
     column(4,
            sliderInput("rate",
                        "Return Rate (in %)",
                        min = 1, max = 30, value = 20, step = 1),
            
            sliderInput("growth",
                        "Growth Rate (in %)",
                        min = 1, max = 30, value = 20, step = 1)
     ),
     column(4,
            sliderInput("years",
                        "Years",
                        min = 1, max = 30, value = 20, step = 1),
            
            selectInput("facet",
                        "Facet?",
                        choices = c("Yes", "No"),
                        selected = "No")
     )
   ),
   
   hr(),
   
   # Show plot and data table
   h3("Timeline"),
   plotOutput("balancePlot"),
   
   h3("Balances"),
   verbatimTextOutput("dataTable")
)

server <- function(input, output) {
   modalities <- reactive({
      init <- 1000  # initial investment amount
      contrib <- 200  # annual contribution amount
      rate <- 0.05  # annual return rate
      growth <- 0.03
      years <- 0:10
      num_years <- length(years)
      
      no_contrib <- rep(0, num_years)  # future values with no annuity
      fixed_contrib <- rep(0, num_years)  # future values with annuity
      growing_contrib <- rep(0, num_years)  # future values with growing annuity
      
      for (yr in years) {
        no_contrib[yr+1] <- init * (1 + rate)^yr  
        fixed_contrib[yr+1] <- no_contrib[yr+1] + contrib * (((1 + rate)^yr - 1) / rate)
        growing_contrib[yr+1] <- 
          no_contrib[yr+1] + contrib * (((1 + rate)^yr - (1 + growth)^yr) / (rate - growth))
      }
      
      modalities <- data.frame(year = years, 
                               no_contrib = no_contrib, 
                               fixed_contrib = fixed_contrib, 
                               growing_contrib = growing_contrib)
      return(modalities)
    })
  
    output$balancePlot <- renderPlot({
      data <- reshape2::melt(modalities(), id.vars = "year")
      colnames(data) <- c('year', 'modality', 'value')
      
      g <- ggplot(data = data, aes(x = year, y = value, col = modality)) +
        geom_line() +
        geom_point() +
        labs(x = "Year", y = "Value", title = "Growth of Investment")
      
      if (input$facet == "Yes") {
        g <- g + facet_grid(. ~ modality) + 
          geom_area(aes(fill = modality, alpha = 0.5), show.legend = FALSE)
      }
      
      return(g)
    })
    
    output$dataTable <- renderPrint({ modalities() })
}

shinyApp(ui = ui, server = server)
