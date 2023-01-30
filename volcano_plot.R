#used to generate the plot in the Volcano tab

library("EnhancedVolcano")
library("textshaping")

volcanoplot <- function(res, genes,FC,pvalue){

  res = as.data.frame(res)
  select = genes
  IDS = res$X
  # FC = 1
  # pvalue = 0.05
  volplot = EnhancedVolcano(res,
                            x = "log2FoldChange",
                            y = "pvalue",
                            lab = IDS,
                            pCutoff = pvalue,
                            FCcutoff = FC,selectLab = select,
                            caption = paste("FC cutoff, ",FC,"; p-value cutoff; ",pvalue, "total = ", nrow(res),"genes",sep = ""),
                            pointSize = 1,
                            boxedLabels = TRUE,
                            subtitle = "")

  return(volplot)
  
  
}

