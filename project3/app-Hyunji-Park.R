# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 3: “State of the Union” Text Analysis
# Author: Hyunji Park
# Date: APR 29, 2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(reshape2)


# ===============================================
# Import data
# ===============================================
dat <- read.csv("state-union-2001-2022.csv")
dat$year <- as.character(dat$year)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("“State of the Union” Text Analysis"),
  fluidRow(
    # replace with your widgets
    column(3,
          
           radioButtons(inputId = "choose", 
                        label = "Use facet", 
                        choices = c("Yes" = TRUE,
                                    "No" = FALSE),
                        selected = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           
           sliderInput(inputId = "number", 
                       label = "Number of word",
                       min = 0,
                       max = 30,
                       value = 10,
                       step = 1)
    ),
    
    # replace with your widgets
    column(3,
           
           radioButtons(inputId = "stopwords", 
                        label = "Removing stopwords", 
                        choices = c("True" = TRUE,
                                    "False" = FALSE),
                        selected = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           
           sliderInput(inputId = "binwidth",
                       label = "Binwidth",
                       min = 1,
                       max = 30,
                       value = 10,
                       step = 1)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("The most common words by president"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Sentiment analysis"),
                       plotOutput("histogram"),
                       hr(),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
  dat_freq <- reactive({
    if(input$stopwords){
      dat %>%
        unnest_tokens(word, message) %>%
        inner_join(get_sentiments("bing")) %>%
        count(president, sentiment, word, sort = TRUE) %>%
        arrange(desc(n)) %>%
        group_by(president) %>%
        top_n(input$number)

    }
    else{
      dat %>%
        unnest_tokens(word, message) %>%
        anti_join(stop_words, by = "word") %>%
        inner_join(get_sentiments("bing")) %>%
        count(president, sentiment, word, sort = TRUE) %>%
        arrange(desc(n)) %>%
        group_by(president) %>%
        top_n(input$number)
    }
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({

    no = ggplot(data = dat_freq(), aes(x = word, y = n, fill = president)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_x_reordered() + 
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ylab("number of words")
    
    yes = ggplot(data = dat_freq(), aes(x = word, y = n, fill = president)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_x_reordered() + facet_wrap( ~ president, scales = "free") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ylab("number of words")
    
    if(input$choose){
      yes
    }else{no}
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({

    dat_freq() %>%
      dcast(word ~ president, value.var = "n")
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for histogram
  output$histogram <- renderPlot({

    no = ggplot(data = dat_freq(), aes(x = n, fill = sentiment)) +
      geom_histogram(bins = input$binwidth, alpha = 0.7, position = "dodge") +
      scale_x_continuous(breaks = seq(0, 100, 5)) +
      xlab("number of words") +
      labs(title = "All presidents") +
      ylab(NULL)
    
    yes = ggplot(data = dat_freq(), aes(x = n, fill = sentiment)) +
      geom_histogram(bins = input$binwidth, alpha = 0.7, position = "dodge") +
      scale_x_continuous(breaks = seq(0, 100, 5)) +
      facet_wrap( ~ president, scale = "free") +
      xlab("number of words") +
      ylab(NULL) +
      labs(title = "By presidents")
    
    if(input$choose){
      yes
    }else{no}
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    
    
    dat_freq() %>%
      dcast(sentiment ~ president)
    
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

