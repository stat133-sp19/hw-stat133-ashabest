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
      modalities <- modalities()
      
      xrange <- c(0, 10)
      yrange <- c(0, 4500) 
      
      # set up plot 
      plot(xrange, yrange, type="n", xlab="year", ylab="value")
      axis(side = 1, at = seq(from = 0, to = 10, by = 1))
      
      # add lines 
      lines(modalities$year, modalities$no_contrib, 
            las = 1, type = "l", lwd = 2, col="#CA0713")
      lines(modalities$year, modalities$fixed_contrib, 
            las = 1, type = "l", lwd = 2, col="#C78920")
      lines(modalities$year, modalities$growing_contrib, 
            las = 1, type = "l", lwd = 2, col="#51991D")
      
      title("Growth of Investment Modes Over 10-Year Period")
      
      legend(xrange[1], yrange[2],
             legend=names(modalities)[-1],
             col=c("#CA0713", "#C78920", "#51991D"),
             lty=1, cex=0.8)
    })
    
    output$dataTable <- renderPrint({ modalities() })
}

shinyApp(ui = ui, server = server)
