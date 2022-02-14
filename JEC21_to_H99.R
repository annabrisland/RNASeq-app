library(tidyverse)

convertGeneID <- function(id) {
  
  geneconv <- read.csv("May_2021_H99_JEC21_orthologs.csv") %>%
    select(Gene.ID, Input.Ortholog.s.)
  colnames(geneconv)[1] = "NAME"
  
  data <- id %>%
  inner_join(geneconv, by = "NAME") %>%
  select(-NAME) %>%
  distinct(Input.Ortholog.s., .keep_all = TRUE)
  data <- data[,c(ncol(data),1:(ncol(data)-1))]
  colnames(data)[1] = "NAME"
  
  
  return(data)
  
}