pathwayEnrichmentUI <- function(id) {
  tabPanel("Pathway Enrichment",
           sidebarLayout(
             sidebarPanel(
               helpText("Upload your node tables (.csv)."),
               fileInput(NS(id,"file1"), "Choose .csv File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               uiOutput(NS(id, "selectfile")), 
               checkboxInput(NS(id, "H99"), "My data is Cryptococcus neofomans (H99)"),              
               actionButton(NS(id, "button"), "Go!"),
               h4("  "),
               numericInput(NS(id, "topn"), "Filter number of pathways:", min = 0, max = 1000, value = 15),
               selectInput(NS(id, "regulation"), "Filter by enriched pathways:", 
                           c("all", "positively", "negatively"), selected = "None"),
               numericInput(NS(id, "pvalue"), "Filter by p value:", min = 0, max = 0.1, value = 0.05),
               numericInput(NS(id, "qvalue"), "Filter by q value:", min = 0, max = 0.1, value = 0.001),
               
               sliderInput(NS(id, "nodesize"), "Filter by gene set size:", min = 0, max = 600, value = c(0, 600)),
               textInput(NS(id, "pathway"), "Filter by key words:", placeholder = "e.g. mitochondria iron"),),
             mainPanel(
               plotOutput(NS(id, "plot1")),
               uiOutput(NS(id, "plotButton")),
               numericInput(NS(id, "pathwaytext_size"), "Change the  y-axis text size", min = 1, max = 50, value = 10),
               h4("  "),
               DT::dataTableOutput(NS(id, "table1")),
               # uiOutput("tableButton"),
               h4("  ")
             ))) 
}