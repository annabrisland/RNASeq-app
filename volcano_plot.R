#used to generate the plot in the Volcano tab

library("EnhancedVolcano")
library("textshaping")

volcanoplot <- function(res, genes){
  
  #testing parameters
  res = read.csv("C:/Users/cwjle/OneDrive - UBC/Desktop/GradWork/Data/collaborations/Jun/HOWT_vs_HOET/HOWT_vs_HOET _Raw_results.csv",header = T)

  
  res.df = as.data.frame(res)
  
  select = genes
  IDS = res.df$X
  
  volplot = EnhancedVolcano(res.df,
                            x = "log2FoldChange",
                            y = "pvalue",
                            lab = IDS,
                            pCutoff = 10e-4,
                            FCcutoff = 2,selectLab = select,
                            caption = paste0('FC cutoff, 2; p-value cutoff, 10e-4; ',"total = ", nrow(res.df), " genes"),
                            pointSize = 1,
                            boxedLabels = TRUE,
                            subtitle = "")

  return(volplot)
  
  
}

