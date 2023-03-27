DEGUI <- function(id) {
  tabPanel("DEG", 
           sidebarLayout(
             sidebarPanel(
               helpText("Upload your expression values (.txt)."),
               fileInput(NS(id, "file2"), "Choose .csv File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               helpText("Upload your completed metadata template."),
               fileInput(NS(id, "file5"), "Choose .csv File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               downloadLink(NS(id, "exportTemplateDEG"), "Download our metadata template here."),
               h4("  "),
               textInput(NS(id, "gene_name"), "Choose a gene:", placeholder = "e.g. CNAG_02780"),
               actionButton(NS(id, "button2"), "Go!"),                    
             ),
             mainPanel(
               plotOutput(NS(id, "plot2")),
               uiOutput(NS(id, "barplotbutton")),
               textInput(NS(id, "yaxis_name"), "y-axis label", placeholder = "Normalized gene expression", value ="Normalized gene expression" ),
               textInput(NS(id, "specify_order"), "Specify the order of your first condition", placeholder = "A, B, C", value ="" ),
               textInput(NS(id, "specify_order2"), "Specify the order of your second condition", placeholder = "A, B, C", value ="" ),
               numericInput(NS(id, "text_size"), "Change the text size", min = 0, max = 50, value = 15),
               helpText("Press Go! after changing the options above")),
             
           ))
}