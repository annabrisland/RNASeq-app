library("tidyverse")
library("dittoSeq")
library("SummarizedExperiment")

plotHeatmap <- function(id) {
  
  exp <- id %>%
    select(-DESCRIPTION) %>%
    distinct(NAME, .keep_all = TRUE)
  rownames(exp) <- exp[,1]   
  
  exp <- select(exp, -NAME) %>%
    head()
  
  treatment <- c(rep("lowGlucose", 3), rep("highGlucose", 3), rep("lowMethionine", 3), rep("highMethionine", 3),
                 rep("lowGlucose", 3), rep("highGlucose", 3), rep("lowMethionine", 3), rep("highMethionine", 3))
  strain <- c(rep("H99", 12), rep("Cir1", 12))
  
  SEbulk <- SummarizedExperiment(assays = exp)
  
  SCEbulk <- importDittoBulk(x = list(counts = exp), metadata = data.frame(treatment = treatment, strain = strain))
  
  dd <- dittoHeatmap(SCEbulk, getGenes(SCEbulk), annot.by = c("treatment", "strain"), cluster_cols = FALSE,
                     show_colnames = FALSE, rowv = FALSE, fontsize = 6.75)
  
  return(dd)
  
}