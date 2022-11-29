library(shiny)
library(tidyverse)

ui <- fluidPage(

  titlePanel("Future Value of Annuity"),

  sidebarLayout(
    sidebarPanel(
      # present value
      numericInput(inputId = "present",
                   label = "Periodic contribution made at the end of each period ($)",
                   min = 1,
                   value = 1000),
      # time (number of years)
      sliderInput(inputId = 'time', 
                  label = 'Time (in years)', 
                  min = 1, 
                  max = 50,
                  value = 5,
                  step = 1),
      # annual rate of return
      sliderInput(inputId = 'rate', 
                  label = 'Rate of return (in %)', 
                  min = 0, 
                  max = 20,
                  value = 5,
                  step = 0.1),
      # ordinary(due = FALSE) or due(due = TRUE)
      radioButtons(inputId = "due",
                   label = "Annuity due",
                   choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                   selected = FALSE)
    ),
    
    # Show a plot of the generated future values
    mainPanel(
      plotOutput("timeline"),
      hr(),
      h4('Future Value'),
      verbatimTextOutput('table')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # table of future values (reactive expression)
  # (notice that "rate" is divided by 100)
  future <- reactive({
    fv = input$present * ((1 + input$rate/100)^(0:input$time) - 1) / (input$rate/100)
    if (input$due){
      fv = fv * (1 + input$rate/100)
    }
    tbl = data.frame(
      year = 0:input$time,
      amount = fv
    )
    tbl
  })
  
  # plot timeline
  # (notice that "future" is reactive)
  output$timeline <- renderPlot({
    ggplot(data = future(), aes(x = year, y = amount)) +
      geom_point() +
      geom_line() + 
      theme_minimal()
  })
  
  # show data frame
  output$table <- renderPrint({
    print(future(), print.gap = 3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
