setwd("C:/Users/small/Documents/Brady lab/Soil Paper")

library(reshape2)
library(pheatmap)


data <- read.csv("FullSoilV3.csv")


data2<-data[-c(15,30,48:50),] #remove artificially infested samples (15 & 48) and outlier E30
row.names(data2)<-data2[,1]
colnames(data2)
data3<-data2[, c(6:37, 43:46)] #Striga and physio-chem
data3m<-data.matrix(data3)

#DataMat<-remove_constant(data3, na.rm = FALSE, quiet = TRUE)
p<-pheatmap(data3m, cluster_cols = TRUE, cluster_rows = TRUE, clustering_distance_rows = "euclidean",
            scale="column",clustering_distance_cols = "euclidean", cutree_rows=6, border_color = "gray80")
p

clusters<-as.data.frame(cutree(p$tree_row, k = 6))
write.csv(clusters, "clusters1.csv")
p<-pheatmap(data3m, cluster_cols = TRUE, cluster_rows = TRUE, clustering_distance_rows = "euclidean",
            scale="column",clustering_distance_cols = "euclidean", cutree_rows=6, border_color = "gray80",
            gaps_col = gp_row, annotation_col = col_ann)

col_ann<-read.csv("Physicochem annotations.csv")
col_ann$Type<-as.factor(col_ann$Type)
rownames(col_ann)<-col_ann[,1]
col_ann<-col_ann[-c(1)]
gp_row = which(diff(as.numeric(factor(col_ann$Type)))!=0)


save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  pdf(filename, width=width, height=height)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}
save_pheatmap_pdf(p, "Heatmap-Soil-PhysicoChem_6clusters.pdf")



########10-3-2024 Without STriga info

data2<-data[-c(30),] #remove outlier E30
row.names(data2)<-data2[,1]
colnames(data2)
data3<-data2[, c(6:37)] #physio-chem
data3m<-data.matrix(data3)

#DataMat<-remove_constant(data3, na.rm = FALSE, quiet = TRUE)
pheatmap(data3m, cluster_cols = TRUE, cluster_rows = TRUE, clustering_distance_rows = "euclidean",
            scale="column",clustering_distance_cols = "euclidean", cutree_rows=6, border_color = "gray80")


col_ann<-read.csv("Physicochem annotations.csv")
col_ann$Type<-as.factor(col_ann$Type)
col_ann<-col_ann[-c(33:36),]
rownames(col_ann)<-col_ann[,1]
col_ann<-col_ann[-c(1)]
gp_row = which(diff(as.numeric(factor(col_ann$Type)))!=0)

p<-pheatmap(data3m, cluster_cols = FALSE, cluster_rows = TRUE, clustering_distance_rows = "euclidean",
            scale="column", cutree_rows=6, border_color = "gray80",
            gaps_col = gp_row)
clusters <- cutree(p$tree_row, k = 6)
data_with_clusters <- cbind(cluster = clusters, data3m)
data_with_clusters<-as.data.frame(data_with_clusters)
cluster_means <- aggregate(. ~ cluster, data = data_with_clusters, FUN = mean)
write.csv(cluster_means, "physiochem cluster means.csv")

## ONLY STRIGA
data2<-data[-c(30, 15, 48:50),] 
row.names(data2)<-data2[,1]
colnames(data2)
data3<-data2[, c(43:46)] #striga
data3m<-data.matrix(data3)
