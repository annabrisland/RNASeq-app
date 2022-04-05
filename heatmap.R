
options(repos = BiocManager::repositories())
library("tidyverse")
library("dittoSeq")
library("SummarizedExperiment")
plotHeatmap <- function(id, meta, list) {

  #For troubleshooting
  # meta = read.csv("metadata_test.csv", skip = 1)
  # id = read.table("_expression_valuesALL.txt", sep = "\t", header = T)
  # list = "CNAG_04242 CNAG_01137 CNAG_03427 CNAG_07908 CNAG_00082 CNAG_02231 CNAG_02358 CNAG_05497 CNAG_03226 CNAG_05011 CNAG_02654 CNAG_07572 CNAG_00462 CNAG_03629 CNAG_02138 CNAG_05041 CNAG_03985 CNAG_06063 CNAG_03352 CNAG_01802 CNAG_05070"

  
  
    genes <- as.data.frame(strsplit(list, split = "\\s+"))
  colnames(genes)[1] <- "NAME"
  
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
  
  SCEbulk <- importDittoBulk(x = list(counts = exp), metadata = data.frame("cluster" = paste(meta[[2]],meta[[3]])))
  
  dd <- dittoHeatmap(SCEbulk, getGenes(SCEbulk), annot.by = c("cluster"), cluster_cols = FALSE,
                     show_colnames = FALSE, rowv = FALSE, fontsize = 6.75, scale = "row", scaled.to.max = FALSE)
  
  return(dd)
  
}


