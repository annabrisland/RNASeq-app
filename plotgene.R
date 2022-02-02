library("tidyverse")
library("gridExtra")
library("ggplot2")
library("reshape2")


plotgene <- function(normalized_counts, geneid) {

normalized_counts =  read.csv("AllGeneCounts.csv")

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

subdata$cond = c("24h_glucose","24h_glucose","24h_glucose","24h_glucose malate","24h_glucose malate","24h_glucose malate","24h_glucose malate stir","24h_glucose malate stir","24h_glucose malate stir","72h_glucose","72h_glucose","72h_glucose","72h_glucose malate","72h_glucose malate","72h_glucose malate","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir","72h_glucose malate stir")
    
    
    
return(     
  ggplot(subdata,aes(x=cond, y= as.numeric(value),fill = cond))+
    geom_point()+
    geom_boxplot()+
    geom_jitter(aes(color = "black"),size = 4)+
    ggtitle(geneid)+
    theme_bw()+
    labs(y="Normalized read count")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = -45),
          axis.text = element_text(face="bold",size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face = "bold",size = 15))
    
  )
}
