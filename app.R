library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
setwd("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")
source("plot.R")
source("filter.R")
source("plotgene.R")

ui <- fluidPage(
  titlePanel("RNA-Seq Visualisation"),
  theme = shinytheme("superhero"),
  fluidRow(
    
    
    fluidRow(column(width = 2,offset = 1, #opening the user input space
                    
                    
                    tabsetPanel(type= "pills", #opening tabs
                                tabPanel("Pathway Enrichment", #tab1

                    helpText("Upload your node tables (.csv) and gene counts (.csv)"),
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    uiOutput("selectfile"),               
                    actionButton("button", "Go!"),
                    h4("  "),             
                                         
                                   
                                         numericInput("topn", "Filter number of pathways:", min = 0, max = 1000, value = 15),
                                         selectInput("regulation", "Filter by regulated pathways:", 
                                                     c("all", "upregulated", "downregulated"), selected = "None"),
                                         numericInput("pvalue", "Filter by p value:", min = 0, max = 0.1, value = 0.05),
                                         sliderInput("nodesize", "Filter by gene set size:", min = 0, max = 600, value = c(0, 600)),
                                         textInput("pathway", "Filter by key word:", placeholder = "e.g. mitochondria"),
                                         
                                ),
                                tabPanel("DEG", #tab2
                                         
                                         
                                         helpText("Upload your gene counts (.csv)"),
                                         fileInput("file2", "Choose CSV File",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         uiOutput("selectfile2"),               
                                         actionButton("button2", "Go!"),
                                         h4("  "),                    
                                         textInput("gene_name", "List of genes:", placeholder = "e.g. CNAG_02780"),
                                )
                    ), # closing tabs
    ), # closing the user input space
    
    
      column(width = 8, #opening the plotting space
             plotOutput("plot1"),
             downloadButton("exportplot", "Save plot"),
             tableOutput("table1"),
             downloadButton("exporttable", "Save table"),
             plotOutput("plot2"),
             tableOutput("counts"),
           
           
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
  
  output$selectfile2 <- renderUI({
    if(is.null(input$file1)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select", "Select File", choices=input$file2$name))
  })
  
  data <- eventReactive(input$button,{
    read.csv(input$file1$datapath[input$file1$name==input$Select])
  })
  
  data2 <- eventReactive(input$button2,{
    read.csv(input$file2$datapath[input$file2$name==input$Select])
  })
  
  output$counts <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath[input$file1$name==input$Select])
    return(head(df))
  })
  
  output$plot1 <-  renderPlot({
    plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)
  })
  
  output$plot2 <-  renderPlot({
    
    plotgene(data2(), input$gene_name)
  })
  
  output$table1 <- renderTable({
    tableNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)
  })
  
  output$export <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      ggsave(file, plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize, input$pathway),height=4,dpi = 120)
    })
  
}

shinyApp(ui = ui, server = server)
