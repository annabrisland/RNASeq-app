library("BiocManager")
library("tidyverse")
library("gridExtra")
library("ggplot2")
library("reshape2")
library("RColorBrewer")

options(repos = BiocManager::repositories())
plotgene <- function(normalized_counts,metadata, geneid) {

# # testing parameters
# normalized_counts =  read.table("_expression_valuesALL.txt",skip = 1)
# geneid = "CNAG_04242"
# metadata = read.csv("metadata_test.csv",skip = 1)
# # testing parameters

normalized_counts = normalized_counts[,-2]
colnames(normalized_counts)[[1]] = "gene_name"

normalized_counts_long <- melt(normalized_counts, id.vars=c("gene_name"))

geneid <- as.data.frame(strsplit(geneid, split = "\\s+"))
colnames(geneid)[1] <- "NAME"
subdata = filter(normalized_counts_long, gene_name %in% geneid$NAME)
subdata = subdata[order(subdata$gene_name),]
subdata$cond <- paste(metadata[[2]],metadata[[3]])

if(length(geneid$NAME) == 1) {
  
  return(     
    ggplot(subdata,aes(x=cond, y= as.numeric(value),fill = cond))+
      geom_point()+
      geom_boxplot()+
      ggtitle("Gene expression")+
      theme_bw()+
      labs(y="Normalized read count")+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = -75),
            axis.text = element_text(face="bold",size=12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold",size = 15))+
      scale_fill_brewer(palette="Paired")
    
  )

} else if(length(geneid$NAME) > 1) {
  
  return(     
    ggplot(subdata,aes(x=gene_name, y= as.numeric(value),fill = cond))+
      geom_boxplot()+
      ggtitle("Gene expression")+
      theme_bw()+
      labs(y="Normalized read count")+
      theme(axis.text.x = element_text(angle = -75),
            axis.text = element_text(face="bold",size=12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold",size = 15))+
      scale_fill_brewer(palette="Paired")
    
  )
  
} #closing loop

} # closing function

