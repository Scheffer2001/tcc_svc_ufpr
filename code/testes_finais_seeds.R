### testes base seeds
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

seeds <- read.table("seeds_dataset.txt")
names(seeds) <- c("area","perimetro","compacidade","comprimento_kernel",
                  "largura_kernel","assimetria","comprimento_sulco_kernel")
plot(seeds[,-8],col=seeds[,8],pch=19)

df_unique <- seeds[!duplicated(seeds[,1:8]), ]
set.seed(123)
tsne_result <- Rtsne(df_unique[,1:7], perplexity = 20,
                     dims = 2, check_duplicates = FALSE,theta=0.25)
tsne_data <- data.frame(
  Dim1 = tsne_result$Y[,1],
  Dim2 = tsne_result$Y[,2],
  Class = df_unique[,8]
)
res_seeds <- SVC(data=tsne_data[,1:2],sigma=0.1022,
                 metric="dunn",scan_n = 30) ## com sigma 0.1022 e scan = 20 deu certo
par(mfrow=c(1,2))
plot(tsne_data[,1:2],col=tsne_data[,3],pch=19)
plot(tsne_data[,1:2],col=res_seeds$clusters,pch=19)
par(mfrow=c(1,2))
plot(seeds[,c(1,2)],col = seeds[,8],pch = 19,cex = 1.3,bty = "n",las = 1)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2],col=res_seeds$clusters,pch = 19,cex = 1.3,
     bty = "n",las = 1)
legend("bottomright",legend=unique(res_seeds$clusters),
       col=unique(res_seeds$clusters),pch=19,cex=1.3,bty='n')
grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_seeds <- kmeans(tsne_data[,1:2], centers = 3)
fcm_seeds <- cmeans(tsne_data[,1:2], centers = 3)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_seeds <- som(as.matrix(tsne_data[,1:2]), grid = som_grid)
som_cluster_seeds <- cutree(hclust(dist(som_seeds$codes[[1]])), k = 3)
hc_seeds <- hclust(dist(tsne_data[,1:2]))
tree_seeds <- cutree(hc_seeds, k = 3)
dbs_seeds <- dbscan(tsne_data[,1:2], eps = 4)

par(mfrow = c(3, 2))
plot(seeds[,2:3], pch=19,col=km_seeds$cluster, 
     main="K-means",bty='n',las=1,cex=1.3)
legend("bottomright",legend=sort(unique(km_seeds$cluster)),col=sort(unique(km_seeds$cluster)),pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,2:3], pch=19, col=fcm_seeds$cluster, 
     main="Fuzzy C-means",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(fcm_seeds$cluster)),col=sort(unique(fcm_seeds$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,2:3], pch=19, col=som_cluster_seeds[som_seeds$unit.classif], 
     main="SOM",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(som_cluster_seeds[som_seeds$unit.classif])),
       col=sort(unique(som_cluster_seeds[som_seeds$unit.classif])), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,2:3], pch=19, col=tree_seeds,main="Árvores de Agrupamento",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(tree_seeds)),col=sort(unique(tree_seeds)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,2:3], pch=19, col=dbs_seeds$cluster+1, main="DBSCAN",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(dbs_seeds$cluster)),col=sort(unique(dbs_seeds$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,2:3],col=res_seeds$clusters,pch=19,main='SVC',
     bty='n',las=1,cex=1.3)
legend("bottomright",legend=c(unique(res_seeds$clusters)),col=unique(res_seeds$clusters),pch=19)
grid(col = "gray80", lty = 2)
methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- tsne_data[,3]
ARI <- c(
  adjustedRandIndex(km_seeds$cluster, y),
  adjustedRandIndex(fcm_seeds$cluster, y),
  adjustedRandIndex(som_cluster_seeds[som_seeds$unit.classif], y),
  adjustedRandIndex(tree_seeds, y),
  adjustedRandIndex(dbs_seeds$cluster, y),
  adjustedRandIndex(res_seeds$clusters, y))
D <- dist(tsne_data[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_seeds$cluster),
                      dunn(distance = D,clusters = fcm_seeds$cluster),
                      dunn(distance = D,clusters = som_cluster_seeds[som_seeds$unit.classif]),
                      dunn(distance = D,clusters = tree_seeds),
                      dunn(distance = D,clusters = dbs_seeds$cluster),
                      dunn(distance = D,clusters = res_seeds$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_seeds$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_seeds$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_seeds[som_seeds$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_seeds)[,3]),
                      mean(silhouette(dist=D,x=dbs_seeds$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_seeds$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)
