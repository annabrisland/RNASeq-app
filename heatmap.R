
options(repos = BiocManager::repositories())
library("tidyverse")
library("dittoSeq")
library("SummarizedExperiment")

plotHeatmap <- function(id, meta, list) {

  genes <- as.data.frame(strsplit(list, split = " "))
  colnames(genes)[1] <- "NAME"
    
  
  strain <- meta$Strain
  treatment <- meta$Treatment
  
  if(list == "") {
    exp <- id %>%
    select(-DESCRIPTION) %>%
    distinct(NAME, .keep_all = TRUE)
  rownames(exp) <- exp[,1]   
  
  exp <- select(exp, -NAME) %>%
    head(n = 15)
  } else {
    exp <- id %>%
      filter(NAME %in% genes$NAME) %>%
      select(-DESCRIPTION) %>%
      distinct(NAME, .keep_all = TRUE)
    rownames(exp) <- exp[,1]   
    
    exp <- select(exp, -NAME)
  }
  
  
  SEbulk <- SummarizedExperiment(assays = exp)
  
  SCEbulk <- importDittoBulk(x = list(counts = exp), metadata = data.frame(treatment = treatment, strain = strain))
  
  dd <- dittoHeatmap(SCEbulk, getGenes(SCEbulk), annot.by = c("treatment", "strain"), cluster_cols = FALSE,
                     show_colnames = FALSE, rowv = FALSE, fontsize = 6.75)
  
  return(dd)
  
}