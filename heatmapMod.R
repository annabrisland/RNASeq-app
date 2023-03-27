heatmapUI <- function(id) {
  tabPanel("Heatmap",
           sidebarLayout(
             sidebarPanel(
               helpText("Upload your expression values (.txt)."),
               fileInput(NS(id,"file3"), "Choose .txt File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               helpText("Upload your completed metadata template."),
               fileInput(NS(id,"file4"), "Choose .csv File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               downloadLink(NS(id,"exportTemplate"), "Download our metadata template here."),
               h4("  "),
               textInput(NS(id,"gene_list"), "Enter your gene list", placeholder = "e.g. CNAG_03012 CNAG_00106 CNAG_00156"),
               actionButton(NS(id,"button4"), "Go!"),
             ),
             mainPanel(
               plotOutput(NS(id,"heatmap")),
               uiOutput(NS(id,"heatmapButton")),
               numericInput(NS(id,"heatmaptext_size"), "Change the gene label text size", min = 1, max = 50, value = 10),
               selectInput(NS(id,"col_cluster"), label = ("How would you like to cluster your heatmap?"), 
                           choices = list("According to column similarity" = TRUE, "According to sample" = FALSE), 
                           selected = TRUE))
             
           ))
}