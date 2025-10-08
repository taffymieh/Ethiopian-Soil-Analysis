library(tidyr)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(openxlsx)

setwd("C:/Users/small/Documents/Brady lab/Soil Paper/Luisa ASVs/Taxa level Correlations UPDATED/Imputed CLR Unknowns Grouped")



pcaAnalysis<-function(df, filename){
  df[is.na(df)]<-"Unknown"
  rownames(df)<-df$X
  df<-df[,-1]
  pca_result <- prcomp(t(df), scale. = TRUE, center = TRUE) #prcomp cant handle NAs
  pc_scores <- pca_result$x
  loadings <- pca_result$rotation
  
  pdf(file= paste(filename, ".pdf", sep=""), width=4, height=4)
  p<-autoplot(pca_result, data = t(df),
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, loadings.label.repel=T) + theme_bw()
  print(p)
  dev.off()
  
  wb <- createWorkbook()
  addWorksheet(wb, "scores")
  addWorksheet(wb, "loadings")
  writeData(wb, "scores", pc_scores, rowNames=TRUE)
  writeData(wb, "loadings", loadings, rowNames=TRUE)
  saveWorkbook(wb, paste(filename, ".xlsx", sep=""), overwrite = TRUE)
  
  sink(paste(filename, ".txt", sep=""))
       print(summary(pca_result))
  sink()
}


#read in clr datas
a<-read.csv("CLR_Bacteria_phylum.csv")
b<-read.csv("CLR_Bacteria_class.csv")
c<-read.csv("CLR_Bacteria_order.csv")
d<-read.csv("CLR_Bacteria_family.csv")
e<-read.csv("CLR_Bacteria_genera.csv")


f<-read.csv("CLR_Fungus_phylum.csv")
g<-read.csv("CLR_Fungus_class.csv")
h<-read.csv("CLR_Fungus_order.csv")
i<-read.csv("CLR_Fungus_family.csv")
j<-read.csv("CLR_Fungus_genera.csv")


#run analysis
pcaAnalysis(a, "PCA_Bacteria_Phylum")
pcaAnalysis(b, "PCA_Bacteria_Class")
pcaAnalysis(c, "PCA_Bacteria_Order")
pcaAnalysis(d, "PCA_Bacteria_Family")
pcaAnalysis(e, "PCA_Bacteria_Genera")

pcaAnalysis(f, "PCA_Fungus_Phylum")
pcaAnalysis(g, "PCA_Fungus_Class")
pcaAnalysis(h, "PCA_Fungus_Order")
pcaAnalysis(i, "PCA_Fungus_Family")
pcaAnalysis(j, "PCA_Fungus_Genera")

#REALIZED THIS ISN'T WHAT I WANT TO ASK
#use imputed & clr transformed ASV counts to run analysis, then compare to taxanomic info of most informative things
pcaAnalysis<-function(df, tax, filename){
  pca_result <- prcomp(t(df), scale. = TRUE, center = TRUE) #prcomp cant handle NAs
  pc_scores <- pca_result$x
  loadings <- as.data.frame(pca_result$rotation)
  loadings$ASV <- rownames(loadings)
  tax$ASV <- rownames(tax)
  loadings_with_tax <- left_join(loadings, tax, by = "ASV")
  row.names(loadings_with_tax)<-loadings_with_tax$ASV
  
  wb <- createWorkbook()
  addWorksheet(wb, "scores")
  addWorksheet(wb, "loadings")
  writeData(wb, "scores", pc_scores, rowNames=TRUE)
  writeData(wb, "loadings", loadings_with_tax, rowNames=TRUE)
  saveWorkbook(wb, paste(filename, ".xlsx", sep=""), overwrite = TRUE)
  
  pdf(file= paste(filename, ".pdf", sep=""), width=4, height=4)
  p<-autoplot(pca_result, data = t(df), label=TRUE, label.repel=T, loadings = FALSE) + theme_bw()
  print(p)
  dev.off()
  
  sink(paste(filename, ".txt", sep=""))
  print(summary(pca_result))
  sink()
}

FunTax<-FUNGUS[,49:54]
BactTax<-BACTERIA[,49:54]
BactTax$ASV<-rownames(BactTax)

pcaAnalysis(CLRFun, FunTax, "PCA_Fungus")
pcaAnalysis(CLRBact, BactTax, "PCA_Bacteria")

pca_res <- prcomp(t(CLRFun), scale. = TRUE, center = TRUE) #prcomp cant handle NAs
loadings <- as.data.frame(pca_res$rotation)
loadings$ASV <- rownames(loadings)
loadings_with_tax <- left_join(loadings, FunTax, by = "ASV")
row.names(loadings_with_tax)<-loadings_with_tax$ASV
top_PC1 <- loadings_with_tax %>% 
  arrange(desc(abs(PC1))) %>% 
  head(10)
View(top_PC1)

pca_res <- prcomp(t(CLRBact), scale. = TRUE, center = TRUE) #prcomp cant handle NAs
loadings <- as.data.frame(pca_res$rotation)
loadings$ASV <- rownames(loadings)
loadings_with_tax <- left_join(loadings, BactTax, by = "ASV")
row.names(loadings_with_tax)<-loadings_with_tax$ASV
top_PC1 <- loadings_with_tax %>% 
  arrange(desc(abs(PC1))) %>% 
  head(20)
View(top_PC1)
