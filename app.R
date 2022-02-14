library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(DT)

#setwd("~/Desktop/RNASeq-app")
#setwd("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")
#setwd("C:/Users/cwjle/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")

source("plot.R")
source("filter.R")
#source("plotgene.R")
source("heatmap.R")


ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(title = "RNA-Seq Visualisation",
             
             tabPanel("About",
                      titlePanel("Hello!"),
                      mainPanel(
                        "This application allows you to visualise your RNA-Seq data",
                         h4("  "),       
                      downloadLink("sample", "Download our sample data to try it out!")
                      )
                      ),
             
             tabPanel("Pathway Enrichment",
                      sidebarLayout(
                      sidebarPanel(
                        helpText("Upload your node tables (.csv)."),
                        fileInput("file1", "Choose .csv File",
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
                        textInput("pathway", "Filter by key word:", placeholder = "e.g. mitochondria"),),
                        mainPanel(
                          plotOutput("plot1"),
                          uiOutput("plotButton"),
                          h4("  "),
                          DT::dataTableOutput("table1"),
                          uiOutput("tableButton"),
                          h4("  ")
                        ))),
             
             tabPanel("DEG", 
                      sidebarLayout(
                        sidebarPanel(
                        helpText("Upload your gene counts (.csv)."),
                      fileInput("file2", "Choose .csv File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      uiOutput("selectfile2"),
                      helpText("Upload your completed metadata template."),
                      fileInput("file4", "Choose .csv File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      downloadLink("exportTemplateDEG", "Download our metadata template here."),
                      h4("  "),
                      actionButton("button2", "Go!"),
                      h4("  "),                    
                      textInput("gene_name", "Choose a gene:", placeholder = "e.g. CNAG_02780"),),
                      mainPanel(
                        plotOutput("plot2")
                      ))),
             
             tabPanel("Heatmap",
                      sidebarLayout(
                        sidebarPanel(
                        helpText("Upload your expression values (.txt)."),
                      fileInput("file3", "Choose .txt File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      uiOutput("selectfile3"),
                      helpText("Upload your completed metadata template."),
                      fileInput("file4", "Choose .csv File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      downloadLink("exportTemplate", "Download our metadata template here."),
                      h4("  "),
                      actionButton("button3", "Build heatmap!"),),
             mainPanel(
               plotOutput("heatmap"))
             ))),
  
)

server <- function(input, output) {
  
  output$selectfile <- renderUI({
    if(is.null(input$file1)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select", "Select File", choices=input$file1$name))
  })
  
  output$selectfile2 <- renderUI({
    if(is.null(input$file2)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select2", "Select File", choices=input$file2$name))
  })
  
  output$selectfile3 <- renderUI({
    if(is.null(input$file3)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select3", "Select File", choices=input$file3$name))
  })
  
  data <- eventReactive(input$button,{
    read.csv(input$file1$datapath[input$file1$name==input$Select])
  })
  
  data2 <- eventReactive(input$button2,{
    read.csv(input$file2$datapath[input$file2$name==input$Select2]) 
  }) 
  
  data3 <- eventReactive(input$button3,{
    read.csv(input$file3$datapath[input$file3$name==input$Select3], sep = "\t") 
  }) 
  
  metadata <- reactive({if(is.null(input$file4)) {return()}
  read.csv(input$file4$datapath, skip = 1)
  })

  output$heatmap <- renderPlot({
    plotHeatmap(data3(), metadata())
  })

  output$plot1 <-  renderPlot({
    plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)
  })
  # 
  # output$plot2 <-  renderPlot({
  #   req(input$file2)
  #   plotgene(data2(), input$gene_name)
  # })

  output$table1 <- DT::renderDataTable({
    tableNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)},
    options = list(bPaginate = F, scrollX = TRUE, scrollY = "500px"))

   output$plotButton <- renderUI({
    req(input$file1)
    downloadButton("exportPlot", "Save plot")
  })

  output$export <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      ggsave(file, plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize, input$pathway),height=4,dpi = 120)
    })

  output$tableButton <- renderUI({
    req(input$file1)
    downloadButton("exportTable", "Save table")
  })

  output$exportTable <- downloadHandler(
    filename = "nodeTable.pdf",
    content = function(file){
      ggsave(file, tableNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize, input$pathway),height=4,dpi = 120)
    })

  output$exportTemplate <- downloadHandler(
    filename = "metadata.csv",
    content = function(file){
      file.copy("metadata_template.csv", file)
    })

  output$exportTemplateDEG <- downloadHandler(
      filename = "metadata.csv",
      content = function(file){
        file.copy("metadata_template.csv", file)
      })

  output$sample <- downloadHandler(
    filename = "sample_nodeTable.csv",
    content = function(file){
      file.copy("0.06gWT_vs_0.06gcir1(fdr0.2)node_table.csv", file)
    })

}

shinyApp(ui = ui, server = server)
