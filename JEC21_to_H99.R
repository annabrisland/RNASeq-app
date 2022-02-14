library(tidyverse)

convertGeneID <- function(id) {
  
  geneconv <- read.csv("May_2021_H99_JEC21_orthologs.csv") %>%
    select(Gene.ID, Input.Ortholog.s.)
  colnames(geneconv)[1] = "NAME"
  
  data <- read.csv("AllSamples _expression_values.txt", sep = "\t") %>%
  inner_join(geneconv, by = "NAME") %>%
  select(-NAME, -DESCRIPTION) %>%
    distinct(Input.Ortholog.s., .keep_all = TRUE)
  rownames(data) <- data$Input.Ortholog.s.
  
  data <- select(data, -Input.Ortholog.s.)
  
  return(data)
  
}