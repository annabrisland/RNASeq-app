library("tidyverse")
library("gridExtra")
library("ggplot2")



plotNode <- function(id, n, reg, pval, size, term) {
  
  if(reg == "upregulated") {
  dataPlot <- id %>%
    filter(NES>=0)
  } else if(reg == "downregulated") {
    dataPlot <- id %>%
      filter(NES<=0)
  } else if(reg == "all") {
    dataPlot <- id
  }
  
 dataPlot <- dataPlot %>%
   filter(pvalue <= pval) %>%
   filter(gs_size <= size)
 
 dataPlot <- dataPlot %>%
   slice_max(order_by = NES, n = n)

 dataPlot$GS_DESCR = factor(dataPlot$GS_DESCR, levels=dataPlot[order(dataPlot$NES), "GS_DESCR"])
 
 if(term == "") {
   dataPlot <- dataPlot
   } else {
    key_values = grep(term,dataPlot$GS_DESCR)
    dataPlot = dataPlot[key_values,]
   }
  
  return(
        ggplot(dataPlot,aes(x=NES,y=GS_DESCR,color = pvalue)) +
          geom_point(aes(size = gs_size)) +
          theme_bw() +
          theme(text = element_text(size=7), axis.text.y = element_text(angle = 0)) +
          labs(y="GO terms",x="Escore", size="gene set size", colour = "P value",
               title = paste("Top", n, reg, "Pathways", sep = " ")) +
          scale_color_gradient(low = "blue", high = "red") +
          scale_y_discrete(labels = function(y) str_wrap(y, width =27)) +
          scale_color_gradient(low = "blue", high = "red")
        )
      
}
