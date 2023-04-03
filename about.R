aboutUI <- function(id) {
  tabPanel("About",
           titlePanel("Hello!"),
           fluidRow(
             column(5,
                    "This application allows you to visualise your RNA-Seq data at the level of pathways and genes.
                        Our 'Pathway Enrichment' tab takes a node table input to provide a dotplot of enriched pathways.
                        In the 'Heatmap' tab, you can build heatmaps to illustrate gene expression data. 
                        Finally, the 'DEG' tab allows you to observe differences in individual gene expression via normalised count data.",
                    h4("  "),       
                    downloadLink(NS(id, "sample_node"), "Click here to download our sample node table to look at pathway enrichment data"),
                    h4("  "),
                    downloadLink(NS(id, "sample_heatmap"), "Click here to download our sample expression data and accompanying metadata to build a heatmap"),
                    h4("  "),
                    downloadLink(NS(id, "sample_DEG"), "Click here to download our sample expression data and accompanying metadata to look at individual gene expression"),
                    h4("  "), 
                    "This webapp was created by Anna Brisland (annabrisland@gmail.com) and Christopher Lee (christopherjlee@msl.ubc.ca)"),
             column(7,
                    img(src='image.png', align = "right", height = "98%", width = "98%"))
           )
  )
}