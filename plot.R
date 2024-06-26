
#Used to plot the GSEA pathway hits in the pathway tab

library("tidyverse")
library("gridExtra")
library("ggplot2")



plotNode <- function(id, n, reg, pval, size1, size2, term, pathwyatext_size, qval) {
  #term <- c("iron mito")
  #term <- unlist(strsplit(term, split = "\\s+"))
  #print(term)
  
  if(term == "") {
    dataPlot <- id
  } else {
    term <- unlist(strsplit(term, split = "\\s+"))
    key_values = grep(paste(term, collapse = "|"),id$GS_DESCR)
    dataPlot = id[key_values,]
  }
  
  if(reg == "positively") {
    dataPlot <- dataPlot %>%
      filter(NES>=0)
  } else if(reg == "negatively") {
    dataPlot <- dataPlot %>%
      filter(NES<=0)
  } else if(reg == "all") {
    dataPlot <- dataPlot
  }
  
  dataPlot <- dataPlot %>%
    filter(pvalue <= pval) %>%
    filter(fdr_qvalue<= qval) %>%
    filter(gs_size >= size1) %>%
    filter(gs_size <= size2)
  
  dataPlot <- dataPlot %>%
    slice_max(order_by = NES, n = n)
  
  dataPlot$GS_DESCR = factor(dataPlot$GS_DESCR, levels=dataPlot[order(dataPlot$NES), "GS_DESCR"])
  
  return(
    ggplot(dataPlot,aes(x=NES,y=GS_DESCR,color = fdr_qvalue)) +
      geom_point(aes(size = gs_size)) +
      theme_bw() +
      theme(text = element_text(size=13), axis.text.y = element_text(angle = 0, size = pathwyatext_size)) +
      labs(y="GO terms",x="Normalized Enrichment score", size="gene set size", colour = "q value",
           title = paste("Top", n, reg, "enriched Pathways", sep = " ")) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_y_discrete(labels = function(y) str_wrap(y, width =50)) +
      scale_color_gradient(low = "blue", high = "red")
  )
  
}
