library("shiny")
library("tidyverse")
library("ggplot2")
library("shinythemes")
library("DT")

# remember to comment these "setwd()" lines out before publishing or else it will break upon deployment

setwd("~/Desktop/RNASeq-app")
#setwd("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")
#setwd("C:/Users/cwjle/OneDrive - UBC/Desktop/GradWork/computational tools/RNAseq_app/RNASeq-app")



#Loading in reference scripts
source("plot.R")
source("filter.R")
source("plotgene.R")
source("heatmap.R")
source("JEC21_to_H99.R")
source("volcano_plot.R")

ui <- fluidPage(
  
  tags$head(includeHTML(("analytics.html"))),
  
  theme = shinytheme("flatly"),
  navbarPage(title = "RNA-Seq Visualisation",
             
             tabPanel("About",
                      titlePanel("Hello!"),
                      fluidRow(
                        column(5,
                        "This application allows you to visualise your RNA-Seq data at the level of pathways and genes.
                        Our 'Pathway Enrichment' tab takes a node table input to provide a dotplot of enriched pathways.
                        In the 'Heatmap' tab, you can build heatmaps to illustrate gene expression data. 
                        Finally, the 'DEG' tab allows you to observe differences in individual gene expression via normalised count data.",
                        h4("  "),       
                        downloadLink("sample_node", "Click here to download our sample node table to look at pathway enrichment data"),
                        h4("  "),
                        downloadLink("sample_heatmap", "Click here to download our sample expression data and accompanying metadata to build a heatmap"),
                        h4("  "),
                        downloadLink("sample_DEG", "Click here to download our sample expression data and accompanying metadata to look at individual gene expression"),
                        h4("  "), 
                        "This webapp was created by Anna Brisland (annabrisland@gmail.com) and Christopher Lee (christopherjlee@msl.ubc.ca)"),
                        column(7,
                        img(src='image.png', align = "right", height = "98%", width = "98%"))
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
                          selectInput("regulation", "Filter by enriched pathways:", 
                                      c("all", "positively", "negatively"), selected = "None"),
                          numericInput("pvalue", "Filter by p value:", min = 0, max = 0.1, value = 0.05),
                          numericInput("qvalue", "Filter by q value:", min = 0, max = 0.1, value = 0.001),
                          
                          sliderInput("nodesize", "Filter by gene set size:", min = 0, max = 600, value = c(0, 600)),
                          textInput("pathway", "Filter by key words:", placeholder = "e.g. mitochondria iron"),),
                        mainPanel(
                          plotOutput("plot1"),
                          uiOutput("plotButton"),
                          numericInput("pathwaytext_size", "Change the  y-axis text size", min = 1, max = 50, value = 10),
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
                          uiOutput("heatmapButton"),
                          numericInput("heatmaptext_size", "Change the gene label text size", min = 1, max = 50, value = 10),
                          selectInput("col_cluster", label = ("How would you like to cluster your heatmap?"), 
                                      choices = list("According to column similarity" = TRUE, "According to sample" = FALSE), 
                                      selected = TRUE))
                        
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
                          textInput("gene_name", "Choose a gene:", placeholder = "e.g. CNAG_02780"),
                          actionButton("button2", "Go!"),                    
                          ),
                        mainPanel(
                          plotOutput("plot2"),
                          uiOutput("barplotbutton"),
                          textInput("yaxis_name", "y-axis label", placeholder = "Normalized gene expression", value ="Normalized gene expression" ),
                          textInput("specify_order", "Specify the order of your first condition", placeholder = "A, B, C", value ="" ),
                          textInput("specify_order2", "Specify the order of your second condition", placeholder = "A, B, C", value ="" ),
                          numericInput("text_size", "Change the text size", min = 0, max = 50, value = 15),
                          helpText("Press Go! after changing the options above")),
                        
                        )),
             
             
             tabPanel("Volcano Plot", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Upload your DESeq2 Results (.csv)."),
                          fileInput("volcano_file", "Choose .csv File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          textInput("volcano_genes", "Choose a gene:", placeholder = "e.g. CNAG_02780"),
                          actionButton("volcano_button", "Go!"),                    
                        ),
                        mainPanel(
                          plotOutput("volcanoplot"),
                          uiOutput("volcanosavebutton"),
                          helpText("Press Go! after changing the options above")),
                        
                      ))),
  
  
  
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
  
  volcano_data <- reactive({read.csv(input$volcano_file$datapath, header = T)
  })
  ### Load in data from user import END
  
  
  
  
  ### TAB for Pathway Enrichment START
   output$plot1 <-  renderPlot({
     validate(need(input$file1, 'Please upload your notetable.'))
      pathway_plot <- plotNode(data(), input$topn, input$regulation, input$pvalue, input$nodesize[1], input$nodesize[2], input$pathway, input$pathwaytext_size,input$qvalue)
      pathway_plot
  }) 
  

  
   output$plotButton <- renderUI({
     validate(need(input$file1, ''))
    downloadButton("exportPlot", "Save plot")
  })

  output$exportPlot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      ggsave(file, plotNode(data(), input$topn, input$regulation,  input$nodesize[1], input$nodesize[2], input$pathway, input$pathwaytext_size,input$qvalue) )
    })
  
  observe(if (input$H99) {
    output$table1 <-  DT::renderDataTable({
      validate(need(input$file1, ""))
    tableNode(convertNode(data()), input$topn, input$regulation, input$nodesize[1], input$nodesize[2], input$pathway,input$qvalue)}, extensions = c('Buttons', 'Scroller'),
    options = list(bPaginate = F, scrollX = TRUE, scrollY = "500px", dom = 'Bfrtip',
                   deferRender = TRUE,
                   buttons = c('csv', 'excel', 'pdf')))
  } else {
    output$table1 <-  DT::renderDataTable({
      validate(need(input$file1, ""))
      tableNode(data(), input$topn, input$regulation, input$pvalue,input$nodesize[1], input$nodesize[2], input$pathway,input$qvalue)}, extensions = c('Buttons', 'Scroller'), 
      options = list(bPaginate = F, scrollX = TRUE, scrollY = "500px", dom = 'Bfrtip',
                     deferRender = TRUE,
                     buttons = c('csv', 'excel', 'pdf')))
  })
  ### TAB for Pathway Enrichment END 
  

  
  ### TAB for heat map plotting START

  v <- reactiveValues(plot = NULL)

  observeEvent(input$button4, {
   v$plot <- plotHeatmap(data3(), metadata(), input$gene_list, input$heatmaptext_size, input$col_cluster)
    
  })
  output$heatmap <- renderPlot({
   if (is.null(v$plot)) return()
      v$plot
    })
  

  output$exportHeatmap <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      
        ggsave(file,plotHeatmap(data3(), metadata(), input$gene_list, input$heatmaptext_size, input$col_cluster))
      
  })
  

  output$heatmapButton <- renderUI({
    req(input$file3)
    downloadButton("exportHeatmap", "Save plot")
  })
    
  
  ### TAB for heat map plotting END
  

 ### TAB for DEG plotting START
 
  x <- reactiveValues(plot = NULL)
  
  

  observeEvent(input$button2, {
    #for testing only
    # valdat2 = read.table("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/Data/2022/RNAseq/peng_30-622029803/2v1/2v1 _expression_values.txt",header = T)
    # valgene = "CNAG_02780 wCNAG_00028"
  
    ###########################################################################
    valdat2 = data2()                                                         #
    valgene = input$gene_name                                                 #
    valgene_name <- as.data.frame(strsplit(valgene, split = "\\s+"))          #
    valgene_name_matrix = as.matrix(valgene_name)                             # validating that the gene names
    vadat2sub = valdat2 %>%                                                   # entered by the user are correct
      filter(valdat2$NAME %in% valgene_name_matrix)                           #
    inputlength = dim(valgene_name)                                           #
    checklength = dim(vadat2sub)                                              #
    ###########################################################################
    if (inputlength[1] == checklength[1]) {
       validate(need(input$gene_name, "Please enter genes"))
       validate(need(data2(), "Please upload the expression file (.txt)"))
       validate(need(metadatagene(), "Please upload the metadata file (.csv)"))
      x$plot <- plotgene(data2(),metadatagene(), input$gene_name, input$yaxis_name, input$text_size, input$specify_order, input$specify_order2)

    } else {
      x$plot <- ""
    }
  })
  
  output$plot2 <- renderPlot({
    validate(need(input$file2, 'Please upload your normalized gene counts.'))
    validate(need(metadatagene(), "Please upload the metadata file (.csv)"))
    validate(need(x$plot!="", "Please double check the gene name spelling and press GO!"))
    x$plot

  })

  
  
  output$exportbarplot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file){
      
      ggsave(file,plotgene(data2(),metadatagene(), input$gene_name,input$yaxis_name, input$text_size,input$specify_order, input$specify_order2))
      
  })
  
  
  output$barplotbutton <- renderUI({
    req(input$file2)
    downloadButton("exportbarplot", "Save plot")
  })
  
  ### TAB for DEG plotting END

  

  ### TAB for volcano plot START  (Under construction!!)
  
  
  vol <- reactiveValues(plot = NULL)

  observeEvent(input$volcano_button, {
    validate(need(input$volcano_file, "Please upload the DESeq2 results (.csv)"))
    vol$plot <- volcanoplot(volcano_data(),input$volcano_genes)

    
  })
  output$volcanoplot <- renderPlot({
    validate(need(input$volcano_file, "Please upload the DESeq2 results (.csv)"))
    validate(need(vol$plot!="", "Please double check the gene name spelling and press GO!"))
    vol$plot
  })

  
  output$exportvolcanoplot <- downloadHandler(
    filename = "volcano_plot.pdf",
    content = function(file){
      
      ggsave(file,volcanoplot(volcano_data(),input$volcano_genes))
      
    })
  
  
  output$volcanosavebutton <- renderUI({
    req(input$volcano_file)
    downloadButton("exportvolcanoplot", "Save plot")
  })
  
  
  ### TAB for volcano plot END
  
  
  
  

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

  output$sample_node <- downloadHandler(
    filename = "Control_versus_TreatmentA_nodeTable_DEMO.csv",
    content = function(file){
      file.copy("demo_data/Control_versus_TreatmentA_nodeTable_DEMO.csv", file)
    })
  
  output$sample_heatmap <- downloadHandler(
    filename = "Allexpression_values_DEMO.zip",
    content = function(file){
      file.copy("demo_data/Allexpression_values_DEMO.zip", file)
    })
  
  output$sample_DEG <- downloadHandler(
    filename = "Control_versus_TreatmentA_expression_values_DEMO.zip",
    content = function(file){
      file.copy("demo_data/Control_versus_TreatmentA_expression_values_DEMO.zip", file)
    })

}

shinyApp(ui = ui, server = server)
