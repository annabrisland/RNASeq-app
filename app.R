#This is a test for pushing changes


library(shiny)
library(tidyverse)
library(ggplot2)
setwd("~/Documents/Projects/Visualisation")
source("plot.R")

ui <- fluidPage(
  titlePanel("RNA-Seq Visualisation"),
  fluidRow(
    
    
    fluidRow(column(width = 5,offset = 1, #opening the user input space
                    helpText("Upload your node tables (.csv) and gene counts (.csv)"),
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    uiOutput("selectfile"),
                    actionButton("button", "Go!"),
                    h4("  "),
                    
                    
                    
                    
                    
                    tabsetPanel(type= "pills", #opening tabs
                                tabPanel("Pathway Enrichment", #tab1
                                         
                                         numericInput("topn", "Filter number of pathways:", min = 0, max = 1000, value = 20),
                                         selectInput("regulation", "Filter by regulated pathways:", 
                                                     c("all", "upregulated", "downregulated"), selected = "None"),
                                         sliderInput("pvalue", "Filter by p value:", min = 0, max = 0.1, value = 0.05),
                                         sliderInput("nodesize", "Filter by node size:", min = 0, max = 1000, value = 1000),
                                         textInput("pathway", "Filter by key word:", placeholder = "e.g. mitochondria"),
                                         
                                         
                                ),
                                tabPanel("DEG", #tab2
                                         
                                         
                                )
                                
                    ), # closing tabs
    ), # closing the user input space
    
    
    column(width = 6, #opening the plotting space
           plotOutput("plot1"),
           downloadButton("export", "Save")
           #tableOutput("list")
           #tableOutput("counts")
           
     ) #closing fluid row 2
    ) #closing fluid row 1
 ) #closing fluid page
)

server <- function(input, output) {
  
 output$selectfile <- renderUI({
    if(is.null(input$file1)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select", "Select File", choices=input$file1$name))
  })
  
  data <- eventReactive(input$button,{
    read.csv(input$file1$datapath[input$file1$name==input$Select]) %>%
      arrange(desc(NES))
  })
  
  output$counts <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath[input$file1$name==input$Select]) %>%
      arrange(desc(NES))
    return(head(df))
  })
 
  output$plot1 <-  renderPlot({plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize, input$pathway)})
  output$list <- renderTable(data())
  
  output$export <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      ggsave(file, plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize, input$pathway),height=4,dpi = 120)
  })
  
}

shinyApp(ui = ui, server = server)
