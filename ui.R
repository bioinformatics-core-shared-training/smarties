
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Experimental Design Demo using Smarties"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("Number","Tube Label"),
      textInput("blue", "Number of Blue Smarties"),
      textInput("non_blue","Number of non-Blue Smarties"),
      checkboxInput("showExpected", "Show Expected Values",value=FALSE),
      checkboxInput("showGroup","Show hidden group information",value=FALSE),
      radioButtons("plotType",label="Type of Plot",choices=c("scatter","boxplot"),selected = "scatter"),
      actionButton("update","Update Table"),
      actionButton("reset", "Reset")
    ),

    # Show a plot of the generated distribution
 #   mainPanel(
#      tableOutput("table1")
#    ),

    mainPanel(
#      tableOutput("table1"),
      plotOutput("freq"),
      verbatimTextOutput("themean")
    )
  )
))
