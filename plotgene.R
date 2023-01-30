
#used to generate the plot in the DEG tab

library("BiocManager")
library("tidyverse")
library("gridExtra")
library("ggplot2")
library("reshape2")
library("RColorBrewer")
library("validate")

# #testing function 
# normalized_counts = read.table("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/Data/2022/RNAseq/peng_30-622029803/2v1/2v1 _expression_values.txt")
# metadata = read.csv("C:/Users/clee41/OneDrive - UBC/Desktop/GradWork/Data/2022/RNAseq/peng_30-622029803/2v1/2v1 _metadata.csv")
# geneid = "CNAG_02780"
# specify_order = "No_LDOPA_WT"
# specify_order2 = "LDOPA_WT"
# yaxis_label = "nothing here"
# text_size = 15
#
#   
options(repos = BiocManager::repositories())
plotgene <- function(normalized_counts,metadata, geneid, yaxis_label, text_size, specify_order, specify_order2) {

# testing parameters
#normalized_counts =  read.table("_expression_valuesALL.txt",skip = 1)
# geneid = "CNAG_04242"
# metadata = read.csv("coldata.csv",skip = 1)
# # testing parameters

normalized_counts = normalized_counts[,-2]
colnames(normalized_counts)[[1]] = "gene_name"
normalized_counts_long <- melt(normalized_counts, id.vars=c("gene_name"))
geneid <- as.data.frame(strsplit(geneid, split = "\\s+"))
colnames(geneid)[1] <- "NAME"



subdata = filter(normalized_counts_long, gene_name %in% geneid$NAME)
subdata = subdata[order(subdata$gene_name),]

#########################################################################
if(is.na(metadata[[2]][1])){                                            #
                                                                        #
  subdata$cond <- metadata[[3]]                                         #
                                                                        #
} else {                                                                # Changing the condition labels based
                                                                        # on the number of conditions.
  subdata$cond <- paste(metadata[[2]],metadata[[3]], sep = "_")         # Currently optimized for 2.
                                                                        #
}                                                                       #
#########################################################################

if(specify_order !="" &specify_order2 !="" ){
  
  
  subdata$cond <- factor(subdata$cond,levels = c(specify_order, specify_order2))
  
} else if(specify_order == "" | specify_order2 == ""){
  
}



if(length(geneid$NAME) == 1) {
  
  return(     
    
    
    ggplot(subdata,aes(x=cond, y= as.numeric(value),fill = cond))+
      geom_point()+
      geom_boxplot()+
      expand_limits(y = 0)+
      ggtitle(geneid)+
      theme_bw()+
      labs(y=yaxis_label)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = -75),
            axis.text = element_text(face="bold",size = text_size),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold",size = text_size))+
      scale_fill_brewer(palette="Paired")
    
  )

} else if(length(geneid$NAME) > 1) {
  
  return(     
    ggplot(subdata,aes(x=gene_name, y= as.numeric(value),fill = cond))+
      geom_boxplot()+
      ggtitle(geneid)+
      expand_limits(y = 0)+
      theme_bw()+
      labs(y=yaxis_label)+
      theme(axis.text.x = element_text(angle = -75),
            axis.text = element_text(face="bold",size = text_size),
            axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold",size = text_size))+
      scale_fill_brewer(palette="Paired")
    
  )
  
} #closing loop

} # closing function

