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

convertNode <-  function(id) {
  
  geneconv <- read.csv("May_2021_H99_JEC21_orthologs.csv") %>%
    select(Gene.ID, Input.Ortholog.s.)
  colnames(geneconv)[1] = "Genes"
  
  nodetable <- id
  
  data <- id %>%
    select("GS_DESCR", "Genes")
  
  pathway_desc <- as.character(data$GS_DESCR)
  
  nodeReform = data.frame()
  
  for (term in pathway_desc) {
    
    spldata <- data %>%
      filter(GS_DESCR == term) %>%
      separate_rows(Genes, sep = "\\|")
    
    convertdata <- spldata %>%
      inner_join(geneconv, by = "Genes") %>%
      select(-Genes) %>%
      separate_rows(Input.Ortholog.s., sep = ",") %>%
      distinct(Input.Ortholog.s., .keep_all = TRUE)
    
    joindata <- rbind(convertdata, comb = apply(convertdata, 2, paste0, collapse = ", ")) %>%
      tail(n = 1) %>%
      mutate(GS_DESCR = term)
    colnames(joindata)[2] <- "Gene list"
    
    nodeReform <- rbind(nodeReform, joindata)
    
  }
  
  newdata <- merge(nodeReform, nodetable) %>%
    select(-Genes)
  colnames(newdata)[2] <- "Genes"
  
  return(newdata)
  
}