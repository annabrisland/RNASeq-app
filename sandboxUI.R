library(shiny)
library(tidyverse)
library(ggplot2)
setwd("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")
source("plot.R")
source("filter.R")
source("plotgene.R")


ui = fluidPage(
  titlePanel("Some rows with border"),
  tags$style(HTML("
      first {
          border: 4px double red;
      }
      second {
          border: 2px dashed blue;
      }
    ")),
  fluidRow(id = "first",
           numericInput("n", "n", 1)
  ),
  fluidRow(
    tags$br()
  ),
  fluidRow(id = "second",
           plotOutput("plot")
  )
)

server = function(input, output, session) {
  output$plot <- renderPlot( plot(head(cars, input$n)) )
}
)
shinyApp(ui = ui, server = server)