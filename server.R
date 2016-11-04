
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
shinyServer(function(input, output,session) {
  df1 <- read.csv("frequencies.csv",stringsAsFactors = FALSE) %>% 
    select(-Allele.Freq)
  
  values <- reactiveValues()
  values$df2 <- data.frame(Number = numeric(0), not.blue=numeric(0),blue=numeric(0))
  
  newEntry <- observe({
    if(input$update > 0) {
#      index <- as.numeric(input$Number)
 #     blue <- as.numeric(input$blue)
  #    nb <- as.numeric(input$non_blue)
   #   newLine <-c(as.numeric(input$Number), as.numeric(input$blue),as.numeric(input$non_blue))
    #  print(newLine)
      isolate(values$df2[nrow(values$df2) + 1,] <- c(as.numeric(input$Number), as.numeric(input$blue),as.numeric(input$non_blue)))
      
    }
  })

  output$table1 <- renderTable({
    
    df <- values$df2
#    df <- mutate(df, Allele.Freq = blue / (blue+not.blue))
    df
    })
})
