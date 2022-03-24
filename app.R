library("shiny")
library("tidyverse")
library("ggplot2")
library("shinythemes")
library("DT")


#setwd("~/Desktop/RNASeq-app")
#setwd("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")
#setwd("C:/Users/cwjle/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")


source("plot.R")
source("filter.R")
source("plotgene.R")
source("heatmap.R")
source("JEC21_to_H99.R")


ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(title = "RNA-Seq Visualisation",
             
             tabPanel("About",
                      titlePanel("Hello!"),
                      mainPanel(
                        "This application allows you to visualise your RNA-Seq data at the level of pathways and genes",
                        h4("  "),       
                        downloadLink("sample", "Download our sample data to try it out!"),
                        h4("  "),      
                        "This webapp was created by Anna Brisland and Christopher Lee (christopherjlee@msl.ubc.ca)"
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
                          checkboxInput("H99", "My data is Cryptococcus neofomans (H99)"),              
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
                          # uiOutput("tableButton"),
                          h4("  ")
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
                          helpText("Upload your completed metadata template."),
                          fileInput("file4", "Choose .csv File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          downloadLink("exportTemplate", "Download our metadata template here."),
                          h4("  "),
                          textInput("gene_list", "Enter your gene list", placeholder = "e.g. CNAG_03012 CNAG_00106 CNAG_00156"),
                          actionButton("button4", "Go!"),
                        ),
                        mainPanel(
                          plotOutput("heatmap"),
                          uiOutput("heatmapButton"))
                      )),
             tabPanel("DEG", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Upload your expression values (.txt)."),
                          fileInput("file2", "Choose .csv File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          helpText("Upload your completed metadata template."),
                          fileInput("file5", "Choose .csv File",
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
                          plotOutput("plot2"),
                          uiOutput("barplotbutton")
                        )))),
  
  
  
)

server <- function(input, output) {
  
### File import code  START
  
  output$selectfile <- renderUI({
    if(is.null(input$file1)) {return()}
    list(hr(), 
         helpText("Select the file you want to analyse"),
         selectInput("Select", "Select File", choices=input$file1$name))
  })
  
  
   metadata <- reactive({if(is.null(input$file4)) {return()}
  read.csv(input$file4$datapath)
  })
   
   metadatagene <- reactive({if(is.null(input$file5)) {return()}
     read.csv(input$file5$datapath)
   })
  
   ### File import code  END
   
   ### Load in data from user import START
  data <- eventReactive(input$button,{
    read.csv(input$file1$datapath[input$file1$name==input$Select])
  })
  
  data2 <- reactive({read.csv(input$file2$datapath, sep = "\t")
    })
  
  
  data3 <- reactive({read.csv(input$file3$datapath, sep = "\t")
    }) 
  ### Load in data from user import END
  
  
  
  
  ### TAB for Pathway Enrichment START
   output$plot1 <-  renderPlot({
     validate(need(input$file1, 'Please upload your notetable.'))
      pathway_plot <- plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)
      pathway_plot
  }) 
  

  
   output$plotButton <- renderUI({

    downloadButton("exportPlot", "Save plot")
  })

  output$exportPlot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      ggsave(file, plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway) )
    })
  
  observe(if (input$H99) {
    output$table1 <-  DT::renderDataTable({
      validate(need(input$file1, ""))
    tableNode(convertNode(data()), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)}, extensions = c('Buttons', 'Scroller'),
    options = list(bPaginate = F, scrollX = TRUE, scrollY = "500px", dom = 'Bfrtip',
                   deferRender = TRUE,
                   buttons = c('csv', 'excel', 'pdf')))
  } else {
    output$table1 <-  DT::renderDataTable({
      validate(need(input$file1, ""))
      tableNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway)}, extensions = c('Buttons', 'Scroller'), 
      options = list(bPaginate = F, scrollX = TRUE, scrollY = "500px", dom = 'Bfrtip',
                     deferRender = TRUE,
                     buttons = c('csv', 'excel', 'pdf')))
  })
  ### TAB for Pathway Enrichment END 
  

  
  ### TAB for heat map plotting START

  v <- reactiveValues(plot = NULL)

  observeEvent(input$button4, {
   v$plot <- 
      plotHeatmap(data3(), metadata(), input$gene_list)
    
  })
  output$heatmap <- renderPlot({
   if (is.null(v$plot)) return()
      v$plot
    })
  

  output$exportHeatmap <- downloadHandler(
    filename = "plot.png",
    content = function(file){
      
        ggsave(file,plotHeatmap(data3(), metadata(), input$gene_list))
      
  })

  
  output$heatmapButton <- renderUI({
    req(input$file3)
    downloadButton("exportHeatmap", "Save plot")
  })
    
  
  ### TAB for heat map plotting END
  

 ### TAB for DEG plotting START
 
  x <- reactiveValues(plot = NULL)
  
  observeEvent(input$button2, {
    x$plot <- 
      plotgene(data2(),metadatagene(), input$gene_name)
  })
  
  output$plot2 <- renderPlot({
    validate(need(input$file2, 'Please upload your normalized gene counts.'))
    if (is.null(x$plot)) return()
    x$plot
  })

  
  
  output$exportbarplot <- downloadHandler(
    filename = "plot.png",
    content = function(file){
      
      plotgene(data2(),metadatagene(), input$gene_name)
      
    })
  
  
  output$barplotbutton <- renderUI({
    req(input$file2)
    downloadButton("exportbarplot", "Save plot")
  })
  
  ### TAB for DEG plotting END

  


  ### MISC


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
