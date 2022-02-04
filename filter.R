library("tidyverse")
library("gridExtra")
library("ggplot2")


tableNode <- function(id, n, reg, pval, size1, size2, term) {
  
  dataSelect <- id %>%
    select("GS_DESCR", "Name", "Genes", "gs_size", "fdr_qvalue", "pvalue", "NES")
  
  dataSelect <- dataSelect[, c("GS_DESCR", "Name", "gs_size", "fdr_qvalue", "pvalue", "NES", "Genes")]
  dataSelect$Genes <- gsub("\\|", ", ", dataSelect$Genes)
  
  if(term == "") {
    dataTable <- dataSelect
  } else {
    key_values = grep(term,dataSelect$GS_DESCR)
    dataTable = dataSelect[key_values,]
  }
  
  if(reg == "upregulated") {
    dataTable <- dataTable %>%
      filter(NES>=0)
  } else if(reg == "downregulated") {
    dataTable <- dataTable %>%
      filter(NES<=0)
  } else if(reg == "all") {
    dataTable <- dataTable
  }
  
  dataTable <- dataTable %>%
    filter(pvalue <= pval)  %>%
    filter(gs_size >= size1) %>%
    filter(gs_size <= size2)
  
  dataTable <- dataTable %>%
    slice_max(order_by = NES, n = n)
  
  dataTable$GS_DESCR = factor(dataTable$GS_DESCR, levels=dataTable[order(dataTable$NES), "GS_DESCR"])
  
  return(dataTable)
  
}
