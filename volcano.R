volcanoUI <- function(id) {
  tabPanel("Volcano Plot", 
           sidebarLayout(
             sidebarPanel(
               helpText("Upload your DESeq2 Results (.csv)."),
               fileInput(NS(id, "volcano_file"), "Choose .csv File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               textInput(NS(id, "volcano_genes"), "Choose a gene:", placeholder = "e.g. CNAG_02780"),
               actionButton(NS(id, "volcano_button"), "Go!"),                    
             ),
             mainPanel(
               plotOutput(NS(id, "volcanoplot")),
               uiOutput(NS(id, "volcanosavebutton")),
               helpText("Press Go! after changing the options above")),
             
           ))
}