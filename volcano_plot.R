#used to generate the plot in the Volcano tab

library("EnhancedVolcano")
library("textshaping")

volcanoplot <- function(res, genes){

  res = as.data.frame(res)
  select = genes
  IDS = res$X
  
  volplot = EnhancedVolcano(res,
                            x = "log2FoldChange",
                            y = "pvalue",
                            lab = IDS,
                            pCutoff = 10e-4,
                            FCcutoff = 2,selectLab = select,
                            caption = paste0('FC cutoff, 2; p-value cutoff, 10e-4; ',"total = ", nrow(res), " genes"),
                            pointSize = 1,
                            boxedLabels = TRUE,
                            subtitle = "")

  return(volplot)
  
  
}

