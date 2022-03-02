options(repos = BiocManager::repositories())
library("tidyverse")
library("gridExtra")
library("ggplot2")
library("reshape2")


plotgene <- function(normalized_counts,metadata, geneid) {

#testing parameters  
# normalized_counts =  read.csv("AllGeneCounts.csv")
# geneid = "CNAG_02780"
# metadata = read.csv("metadata_test.csv",skip = 1)
#testing parameters

normalized_counts$gene_name = rownames(normalized_counts)
normalized_counts$gene_name = as.factor(normalized_counts$gene_name)
normalized_counts$gene_name = normalized_counts$X

normalized_counts_long <- melt(normalized_counts, id.vars=c("gene_name"))


if(geneid == "") {
  print("Please input at least one gene")
} else {
  subdata = normalized_counts_long %>%
    filter(gene_name == geneid)
  subdata = subdata[-1,]
  
}

labels <- paste(metadata[[2]],metadata[[3]])
subdata$cond = labels
    
    
    
return(     
  ggplot(subdata,aes(x=cond, y= as.numeric(value),fill = cond))+
    geom_point()+
    geom_boxplot()+
    geom_jitter(aes(color = "black"),size = 4)+
    ggtitle(geneid)+
    theme_bw()+
    labs(y="Normalized read count")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = -75),
          axis.text = element_text(face="bold",size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face = "bold",size = 15))
    
  )
}
