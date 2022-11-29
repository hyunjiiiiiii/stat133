# Title: Project2: Savings Rate Calculator
# Author: Hyunji Park
# Date: ARR 6, 2022

library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Future Value of Ordinary Annuity"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           numericInput(inputId = "income",
                        label = "Annual income ($)",
                        min = 1,
                        value = 50000)),
    # Input(s) for target-amount
    column(3,
          numericInput(inputId = "target",
                       label = "Target amount ($)",
                       min = 1,
                       value = 1000000)),
    # Input(s) for current-age
    column(3,
           numericInput(inputId = "age",
                        label = "Current age",
                        min = 0,
                        value = 25)),
    # Input(s) for rate-of-return
    column(3,
           sliderInput(inputId = "rate",
                       label = "Rate of return (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1))
  ),
  
  hr(),
  h4("The relationship between savings-rates and number of years to reach a target amount"),
  plotOutput('plot1'),
  
  hr(),
  h4("The \"total contributions\" and the \"total growth\" for various savings rates."),
  plotOutput('plot2'),
  
  hr(),
  h4("Table of Numeric Outputs."),
  DT::dataTableOutput('table')
)


server <- function(input, output){
  dat <- reactive({
    c = input$income * seq(0.05:1, by = 0.05)
    t = log(((input$rate / 100 * input$target) / c) + 1) / log((1 + input$rate / 100))
    total_contribution = c * t
    percent_contribution = round((total_contribution / input$target) * 100, 2)
    tbl = data.frame(
      saving_rate = seq(0.05:1, by = 0.05) * 100,
      annual_contribution = c,
      total_contribution = round(total_contribution, 1),
      total_growth = round(input$target - total_contribution, 1),
      percent_contribution = percent_contribution,
      percent_growth = 100 - percent_contribution,
      number_of_year = round(t, 2),
      age_at_target = round(input$age + t, 1)
    )
  })
  
  # code for plot-1
  output$plot1 <- renderPlot({
    ggplot(data = dat(), aes(x = saving_rate, y = number_of_year)) + 
      geom_bar(stat = "identity", fill = "#5AB2FF") +
      xlab("saving rates (%)") +
      ylab("years to reach target amount")
  })
  
  # code for plot-2
  output$plot2 <- renderPlot({
    ggplot(data = dat(), aes(x = percent_contribution, y = percent_growth, label = saving_rate)) +
      geom_line() + 
      geom_label() + 
      xlab("proportions of contribution (%)") +
      ylab("proportions of growth (%)")
  })
  
  # code for statistics
  output$table <- DT::renderDataTable({
    dat()
  })
  
}

shinyApp(ui = ui, server = server)