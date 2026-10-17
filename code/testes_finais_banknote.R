## Aplicação SVC banknote
setwd('C:/Users/lucas/OneDrive/Área de Trabalho/Estudo/UFPR/TCC/rcode')
library(SVcluster)
library(dplyr)
library(class)
library(dbscan)
library(kohonen)
library(mclust)
library(e1071)
library(clValid)
library(Rtsne)
library(cluster)
library(ggplot2)
options(device='x11')
df <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt',sep = ',')
colnames(df) <- c("variance","skewness","curtosis","entropy","class")
df_unique <- df[!duplicated(df[,1:4]), ]
set.seed(123)
tsne_result <- Rtsne(df_unique[,1:4], perplexity = 100,
                     dims = 2, check_duplicates = FALSE,theta=0.25)
tsne_data <- data.frame(
  Dim1 = tsne_result$Y[,1],
  Dim2 = tsne_result$Y[,2],
  Class = df_unique$class
)
# plot(df_unique[,1:2],col=df_unique[,5]+1,pch=19,cex=1.3,
#      ylab="Assimetria",xlab="Variancia",las=1,bty='n')
# legend('bottomright',legend=unique(df_unique[,5]),
#        col=unique(df_unique[,5]+1),pch=19)
# plot(df_unique[,1:4],col=df_unique[,5]+1,pch=19,cex=1.3)
# 
# ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = factor(Class))) +
#   geom_point(alpha = 0.7, size = 2) +
#   labs(title = "Visualização t-SNE",
#        subtitle = "Dados de autenticação de cédulas",
#        x = "Dimensão 1", y = "Dimensão 2",col="Classe") +
#   scale_color_manual(values = c("0" = "blue", "1" = "red")) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# plot(tsne_data[,1:2],col=tsne_data[,3]+1,pch=19,cex=1.3,las=1,bty='n',
#      xlab="Dimensão 1",ylab="Dimensão 2")
# legend('topright',legend=unique(df_unique[,5]),
#        col=unique(df_unique[,5]+1),pch=19)
res_tsne <- SVC(data=tsne_data[,1:2],scan_n = 10,kernel = "gaussian",
                sigma=0.5) ## com sigma 0.5 e scan_n=10 deu "certo"
# par(mfrow=c(1,2))
# plot(df_unique[,1:2],col=df_unique$class+1,pch=19,cex=1.3,main="Banknote: Rótulos Originais")
# legend('bottomright',legend=unique(df_unique$class+1),col=unique(df_unique$class+1),pch=19,cex=1)
# plot(df_unique[,1:2],col=res_tsne$clusters,pch=19,cex=1.3,main="Banknote: SVC")
# legend('bottomright',legend=unique(res_tsne$clusters),col=unique(res_tsne$clusters),pch=19,cex=1)
# par(mfrow=c(1,2))
# plot(tsne_data[,1:2],col=df_unique$class+1,pch=19,cex=1.3,main="t-SNE Banknote: Rótulos originais")
# plot(tsne_data[,1:2],col=res_tsne$clusters,pch=19,cex=1.3,main="t-SNE Banknote: SVC")
# adjustedRandIndex(x = res_tsne$clusters,df_unique$class+1)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_banknote <- kmeans(tsne_data[,1:2], centers = 2)
fcm_banknote <- cmeans(tsne_data[,1:2], centers = 2)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_banknote <- som(as.matrix(tsne_data[,1:2]), grid = som_grid)
som_cluster_banknote <- cutree(hclust(dist(som_banknote$codes[[1]])), k = 2)
hc_banknote <- hclust(dist(tsne_data[,1:2]))
tree_banknote <- cutree(hc_banknote, k = 2)
dbs_banknote <- dbscan(tsne_data[,1:2], eps = 3)

par(mfrow = c(2,3))
plot(tsne_data[,1:2], pch=19,col=km_banknote$cluster, main="K-means",bty='n',las=1,cex=1.3)
legend("topright",legend=sort(unique(km_banknote$cluster)),col=sort(unique(km_banknote$cluster)),pch=19)
# grid(col = "gray80", lty = 2)
plot(tsne_data[,1:2], pch=19, col=fcm_banknote$cluster, main="Fuzzy C-means",bty='n',las=1,cex=1.3)
legend("topright", legend=sort(unique(fcm_banknote$cluster)),col=sort(unique(fcm_banknote$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(tsne_data[,1:2], pch=19, col=som_cluster_banknote[som_banknote$unit.classif], main="SOM",bty='n',las=1,cex=1.3)
legend("topright", legend=sort(unique(som_cluster_banknote[som_banknote$unit.classif])),
       col=sort(unique(som_cluster_banknote[som_banknote$unit.classif])), pch=19)
# grid(col = "gray80", lty = 2)
plot(tsne_data[,1:2], pch=19, col=tree_banknote, main="Árvores de Agrupamento",bty='n',las=1,cex=1.3)
legend("topright", legend=sort(unique(tree_banknote)),col=sort(unique(tree_banknote)), pch=19)
# grid(col = "gray80", lty = 2)
plot(tsne_data[,1:2], pch=19, col=dbs_banknote$cluster, main="DBSCAN",bty='n',las=1,cex=1.3)
legend("topright", legend=sort(unique(dbs_banknote$cluster)),col=sort(unique(dbs_banknote$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(tsne_data[,1:2],col=res_tsne$clusters,pch=19,main='SVC',bty='n',las=1,cex=1.3)
legend("topright",legend=c(unique(res_tsne$clusters)),col=unique(res_tsne$clusters),pch=19)
# grid(col = "gray80", lty = 2)

par(mfrow = c(2,3))
plot(df_unique[,1:2], pch=19,col=km_banknote$cluster, main="K-means",bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
legend("bottomright",legend=sort(unique(km_banknote$cluster)),col=sort(unique(km_banknote$cluster)),pch=19)
# grid(col = "gray80", lty = 2)
plot(df_unique[,1:2], pch=19, col=fcm_banknote$cluster, main="Fuzzy C-means",bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
legend("bottomright", legend=sort(unique(fcm_banknote$cluster)),col=sort(unique(fcm_banknote$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(df_unique[,1:2], pch=19, col=som_cluster_banknote[som_banknote$unit.classif], main="SOM",bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
legend("bottomright", legend=sort(unique(som_cluster_banknote[som_banknote$unit.classif])),
       col=sort(unique(som_cluster_banknote[som_banknote$unit.classif])), pch=19)
# grid(col = "gray80", lty = 2)
plot(df_unique[,1:2], pch=19, col=tree_banknote, main="Árvores de Agrupamento",bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
legend("bottomright", legend=sort(unique(tree_banknote)),col=sort(unique(tree_banknote)), pch=19)
# grid(col = "gray80", lty = 2)
plot(df_unique[,1:2], pch=19, col=dbs_banknote$cluster, main="DBSCAN",bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
legend("bottomright", legend=sort(unique(dbs_banknote$cluster)),col=sort(unique(dbs_banknote$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(df_unique[,1:2],col=res_tsne$clusters,pch=19,main='SVC',bty='n',las=1,cex=1.3,ylim=c(-18,18),xlim=c(-10,10))
# grid(col = "gray80", lty = 2)
legend("bottomright",legend=c(unique(res_tsne$clusters[res_tsne$clusters <7])),col=unique(res_tsne$clusters[res_tsne$clusters <7]),pch=19)
methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- tsne_data[,3]
ARI <- c(
  adjustedRandIndex(km_banknote$cluster, y),
  adjustedRandIndex(fcm_banknote$cluster, y),
  adjustedRandIndex(som_cluster_banknote[som_banknote$unit.classif], y),
  adjustedRandIndex(tree_banknote, y),
  adjustedRandIndex(dbs_banknote$cluster, y),
  adjustedRandIndex(res_tsne$clusters, y))
D <- dist(tsne_data[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_banknote$cluster),
                      dunn(distance = D,clusters = fcm_banknote$cluster),
                      dunn(distance = D,clusters = som_cluster_banknote[som_banknote$unit.classif]),
                      dunn(distance = D,clusters = tree_banknote),
                      dunn(distance = D,clusters = dbs_banknote$cluster),
                      dunn(distance = D,clusters = res_tsne$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_banknote$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_banknote$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_banknote[som_banknote$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_banknote)[,3]),
                      mean(silhouette(dist=D,x=dbs_banknote$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_tsne$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)

## y é a classe original, res_tsne$clusters é o resultado do SVC
# contingency_table <- table(res_tsne$clusters,y)
# sum_comb_rows <- sum(choose(rowSums(contingency_table), 2))
# sum_comb_cols <- sum(choose(colSums(contingency_table), 2))
# sum_comb_cells <- sum(choose(contingency_table, 2))
# n <- sum(contingency_table)
# total_pairs <- choose(n, 2)
# index <- sum_comb_cells
# expected_index <- sum_comb_rows * sum_comb_cols / total_pairs
# max_index <- 0.5 * (sum_comb_rows + sum_comb_cols)
# ari <- (index - expected_index) / (max_index - expected_index)
# ari
