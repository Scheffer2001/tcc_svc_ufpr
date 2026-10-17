setwd('C:/Users/lucas/OneDrive/Área de Trabalho/Estudo/UFPR/TCC/rcode')
library(SVcluster)
library(dplyr)
library(class)
library(dbscan)
library(kohonen)
library(mclust)
library(e1071)
library(clValid)
library(cluster)

options(device='x11')
smile <- read.csv("dataset_sorriso.csv")
smile_data <- sample_n(smile,size=200)
y_label <- smile_data[,3]
plot(smile_data[, 1:2],col = y_label,pch = 19,cex = 1.3,
     xlab = "x",ylab = "y",bty = "n",las = 1)
legend("bottomright", legend = unique(y_label)[order(unique(y_label))],
  col = unique(y_label)[order(unique(y_label))],pch = 19,bty = "n")
grid(col = "gray80", lty = 2)


# Clusterizacao - Base Smile

### aplicando o SVC
res <- SVC(data = smile_data,sigma=3,scan_n=10)

# par(mfrow=c(1,2))
# plot(smile_data[, 1:2],col = y_label,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("bottomright", legend = unique(y_label)[order(unique(y_label))],
#        col = unique(y_label)[order(unique(y_label))],pch = 19,cex=1.3,bty='n')
# grid(col = "gray80", lty = 2)
# plot(smile_data[,1:2],col=res$clusters,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("bottomright",legend=unique(res$clusters),col=unique(res$clusters),
#        pch=19,cex=1.3,bty='n')
# grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_smile <- kmeans(smile_data[,1:2], centers = 3)
fcm_smile <- cmeans(smile_data[,1:2], centers = 3)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_smile <- som(as.matrix(smile_data[,1:2]), grid = som_grid)
som_cluster_smile <- cutree(hclust(dist(som_smile$codes[[1]])), k = 3)
hc_smile <- hclust(dist(smile_data[,1:2]))
tree_smile <- cutree(hc_smile, k = 3)
dbs_smile <- dbscan(smile_data[,1:2], eps = 0.38)

par(mfrow = c(2, 3))
plot(smile_data[,1:2], pch=19,col=km_smile$cluster, main="K-means",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=sort(unique(km_smile$cluster)),col=sort(unique(km_smile$cluster)),pch=19)
# grid(col = "gray80", lty = 2)
plot(smile_data[,1:2], pch=19,col=fcm_smile$cluster, main="Fuzzy C-means",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=sort(unique(fcm_smile$cluster)),col=sort(unique(fcm_smile$cluster)),pch=19)
# grid(col = "gray80", lty = 2)
plot(smile_data[,1:2], pch=19,col=som_cluster_smile[som_smile$unit.classif], main="SOM",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=sort(unique(som_cluster_smile[som_smile$unit.classif])),
       col=sort(unique(som_cluster_smile[som_smile$unit.classif])),pch=19)
# grid(col = "gray80", lty = 2)
plot(smile_data[,1:2], pch=19,col=tree_smile, main="Árvores de agrupamento",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=sort(unique(tree_smile)),col=sort(unique(tree_smile)),pch=19)
# grid(col = "gray80", lty = 2)
plot(smile_data[,1:2], pch=19,col=dbs_smile$cluster+1, main="DBSCAN",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=sort(unique(dbs_smile$cluster)+1),col=sort(unique(dbs_smile$cluster)+1),pch=19)
# grid(col = "gray80", lty = 2)
plot(smile_data[,1:2],col=res$clusters,pch=19,main="SVC ",bty='n',las=1,xlim=c(-4,4))
legend("bottomright",legend=c(unique(res$clusters)),col=unique(res$clusters),pch=19)
# grid(col = "gray80", lty = 2)

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- smile_data[,3]
ARI <- c(
  adjustedRandIndex(km_smile$cluster, y),
  adjustedRandIndex(fcm_smile$cluster, y),
  adjustedRandIndex(som_cluster_smile[som_smile$unit.classif], y),
  adjustedRandIndex(tree_smile, y),
  adjustedRandIndex(dbs_smile$cluster, y),
  adjustedRandIndex(res$clusters, y))
D <- dist(smile_data[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_smile$cluster),
                dunn(distance = D,clusters = fcm_smile$cluster),
                dunn(distance = D,clusters = som_cluster_smile[som_smile$unit.classif]),
                dunn(distance = D,clusters = tree_smile),
                dunn(distance = D,clusters = dbs_smile$cluster),
                dunn(distance = D,clusters = res$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_smile$cluster)[,3]),
                mean(silhouette(dist=D,x=fcm_smile$cluster)[,3]),
                mean(silhouette(dist=D,x=som_cluster_smile[som_smile$unit.classif]+1)[,3]),
                mean(silhouette(dist=D,x=tree_smile)[,3]),
                mean(silhouette(dist=D,x=dbs_smile$cluster)[,3]),
                mean(silhouette(dist=D,x=res$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)



# Clusterizacao Two Rings
two_rings <- read.csv("two_rings_dataset.csv")
data_two_rings <- sample_n(two_rings,size=400) ### fiz com 400 obs e deu certo
y_label <- data_two_rings[,3]+1
# plot(data_two_rings[, 1:2],col = y_label,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("topright", legend = unique(y_label)[order(unique(y_label))],
#        col = unique(y_label)[order(unique(y_label))],pch = 19)
# grid(col = "gray80", lty = 2)

### aplicando o SVC

res <- SVC(data=data_two_rings[,1:2],kernel = "gaussian",
           sigma = 5,scan_n = 100) ### fiz com 400 obs e sigma = 5 e deu certo, 100 de scan_n
data_two_rings$svc <- res$clusters
data_two_rings <- data_two_rings %>% filter(svc<=2)
# par(mfrow=c(1,2))
# plot(data_two_rings[, 1:2],col = data_two_rings$class+1,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1,xlim=c(-3.5,3.5))
# legend("bottomright", legend = unique(data_two_rings$class+1)[order(unique(data_two_rings$class+1))],
#        col = unique(data_two_rings$class+1)[order(unique(data_two_rings$class+1))],pch = 19,cex=1.3)
# grid(col = "gray80", lty = 2)
# plot(data_two_rings[,1:2],col=data_two_rings$svc,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1,xlim=c(-3.5,3.5))
# legend("bottomright",legend=unique(data_two_rings$svc),col=unique(data_two_rings$svc),
#        pch=19,cex=1.3)
# grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_two_rings <- kmeans(data_two_rings[,1:2], centers = 2)
fcm_two_rings <- cmeans(data_two_rings[,1:2], centers = 2)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_two_rings <- som(as.matrix(data_two_rings[,1:2]), grid = som_grid)
som_cluster_two_rings <- cutree(hclust(dist(som_two_rings$codes[[1]])), k = 2)
hc_two_rings <- hclust(dist(data_two_rings[,1:2]))
tree_two_rings <- cutree(hc_two_rings, k = 2)
dbs_two_rings <- dbscan(data_two_rings[,1:2], eps = 0.35)

par(mfrow = c(2, 3))
plot(data_two_rings[,1:2], pch=19,col=km_two_rings$cluster, main="K-means",xlim=c(-3.5,3.5))
legend("bottomleft",legend=sort(unique(km_two_rings$cluster)),col=sort(unique(km_two_rings$cluster)),pch=19,bty='n')
# grid(col = "gray80", lty = 2)
plot(data_two_rings[,1:2], pch=19,col=fcm_two_rings$cluster, main="Fuzzy C-means",xlim=c(-3.5,3.5))
legend("bottomleft",legend=sort(unique(fcm_two_rings$cluster)),col=sort(unique(fcm_two_rings$cluster)),pch=19,bty='n')
# grid(col = "gray80", lty = 2)
plot(data_two_rings[,1:2], pch=19,col=som_cluster_two_rings[som_two_rings$unit.classif], main="SOM",xlim=c(-3.5,3.5))
legend("bottomleft",legend=sort(unique(som_cluster_two_rings[som_two_rings$unit.classif])),
       col=sort(unique(som_cluster_two_rings[som_two_rings$unit.classif])),pch=19,bty='n')
# grid(col = "gray80", lty = 2)
plot(data_two_rings[,1:2], pch=19,col=tree_two_rings, main="Árvores de agrupamento",xlim=c(-3.5,3.5))
legend("bottomleft",legend=sort(unique(tree_two_rings)),col=sort(unique(tree_two_rings)),pch=19,bty='n')
# grid(col = "gray80", lty = 2)
plot(data_two_rings[,1:2], pch=19,col=dbs_two_rings$cluster+1, main="DBSCAN",xlim=c(-3.5,3.5))
legend("bottomleft",legend=sort(unique(dbs_two_rings$cluster+1)),col=sort(unique(dbs_two_rings$cluster+1)),pch=19,bty='n')
# grid(col = "gray80", lty = 2)
plot(data_two_rings[,1:2],col=data_two_rings$svc,pch=19,main="SVC",xlim=c(-3.5,3.5))
legend("bottomleft",legend=c(unique(data_two_rings$svc)),col=unique(data_two_rings$svc),pch=19,bty='n')
# grid(col = "gray80", lty = 2)

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- data_two_rings[,3]
ARI <- c(
  adjustedRandIndex(km_two_rings$cluster, y),
  adjustedRandIndex(fcm_two_rings$cluster, y),
  adjustedRandIndex(som_cluster_two_rings[som_two_rings$unit.classif], y),
  adjustedRandIndex(tree_two_rings, y),
  adjustedRandIndex(dbs_two_rings$cluster, y),
  adjustedRandIndex(data_two_rings$svc, y))
D <- dist(data_two_rings[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_two_rings$cluster),
                      dunn(distance = D,clusters = fcm_two_rings$cluster),
                      dunn(distance = D,clusters = som_cluster_two_rings[som_two_rings$unit.classif]),
                      dunn(distance = D,clusters = tree_two_rings),
                      dunn(distance = D,clusters = dbs_two_rings$cluster),
                      dunn(distance = D,clusters = data_two_rings$svc)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_two_rings$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_two_rings$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_two_rings[som_two_rings$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_two_rings)[,3]),
                      mean(silhouette(dist=D,x=dbs_two_rings$cluster)[,3]),
                      mean(silhouette(dist=D,x=data_two_rings$svc)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)



# Clusterizacao - Base Two Moons
two_moons <- read.csv("dataset_two_moons.csv")
data_two_moons <- sample_n(two_moons,size=150)
y_label <- data_two_moons[,3]+1
# plot(data_two_moons[, 1:2],col = y_label,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("bottomright", legend = unique(y_label)[order(unique(y_label))],
#        col = unique(y_label)[order(unique(y_label))],pch = 19)
# grid(col = "gray80", lty = 2)

### aplicando o SVC
res <- SVC(data=data_two_moons[,1:2],sigma_candidates = seq(5,6,length.out=15),
           scan_n = 20,kernel = "gaussian",metric = "dunn")

# par(mfrow=c(1,2))
# plot(data_two_moons[, 1:2],col = y_label,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("bottomright", legend = unique(y_label)[order(unique(y_label))],
#        col = unique(y_label)[order(unique(y_label))],pch = 19,bty = "n",cex=1.3)
# grid(col = "gray80", lty = 2)
# plot(data_two_moons[,1:2],col=res$clusters,pch = 19,cex = 1.3,
#      xlab = "x",ylab = "y",bty = "n",las = 1)
# legend("bottomright",legend=unique(res$clusters),col=unique(res$clusters),
#        pch=19,cex=1.3,bty='n')
# grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_two_moons <- kmeans(data_two_moons[,1:2], centers = 2)
fcm_two_moons <- cmeans(data_two_moons[,1:2], centers = 2)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_two_moons <- som(as.matrix(data_two_moons[,1:2]), grid = som_grid)
som_cluster_two_moons <- cutree(hclust(dist(som_two_moons$codes[[1]])), k = 2)
hc_two_moons <- hclust(dist(data_two_moons[,1:2]))
tree_two_moons <- cutree(hc_two_moons, k = 2)
dbs_two_moons <- dbscan(data_two_moons[,1:2], eps = 0.28)

par(mfrow = c(2, 3))
plot(data_two_moons[,1:2], pch=19,col=km_two_moons$cluster, main="K-means",bty='n',las=1,cex=1.3)
legend(x=1.5,y=1,legend=sort(unique(km_two_moons$cluster)),col=sort(unique(km_two_moons$cluster)),pch=19)
# grid(col = "gray80", lty = 2)
plot(data_two_moons[,1:2], pch=19, col=fcm_two_moons$cluster, main="Fuzzy C-means",bty='n',las=1,cex=1.3)
legend(x=1.5,y=1, legend=sort(unique(fcm_two_moons$cluster)),col=sort(unique(fcm_two_moons$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(data_two_moons[,1:2], pch=19, col=som_cluster_two_moons[som_two_moons$unit.classif], main="SOM",bty='n',las=1,cex=1.3)
legend(x=1.5,y=1, legend=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])),
       col=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])), pch=19)
# grid(col = "gray80", lty = 2)
plot(data_two_moons[,1:2], pch=19, col=tree_two_moons, main="Árvores de Agrupamento",bty='n',las=1,cex=1.3)
legend(x=1.5,y=1, legend=sort(unique(tree_two_moons)),col=sort(unique(tree_two_moons)), pch=19)
# grid(col = "gray80", lty = 2)
plot(data_two_moons[,1:2], pch=19, col=dbs_two_moons$cluster, main="DBSCAN",bty='n',las=1,cex=1.3)
legend(x=1.5,y=1, legend=sort(unique(dbs_two_moons$cluster)),col=sort(unique(dbs_two_moons$cluster)), pch=19)
# grid(col = "gray80", lty = 2)
plot(data_two_moons[,1:2],col=res$clusters,pch=19,main='SVC',bty='n',las=1,cex=1.3)
legend(x=1.5,y=1,legend=c(unique(res$clusters)),col=unique(res$clusters),pch=19)
# grid(col = "gray80", lty = 2)

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- data_two_moons[,3]
ARI <- c(
  adjustedRandIndex(km_two_moons$cluster, y),
  adjustedRandIndex(fcm_two_moons$cluster, y),
  adjustedRandIndex(som_cluster_two_moons[som_two_moons$unit.classif], y),
  adjustedRandIndex(tree_two_moons, y),
  adjustedRandIndex(dbs_two_moons$cluster, y),
  adjustedRandIndex(res$clusters, y))
D <- dist(data_two_moons[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_two_moons$cluster),
                      dunn(distance = D,clusters = fcm_two_moons$cluster),
                      dunn(distance = D,clusters = som_cluster_two_moons[som_two_moons$unit.classif]),
                      dunn(distance = D,clusters = tree_two_moons),
                      dunn(distance = D,clusters = dbs_two_moons$cluster),
                      dunn(distance = D,clusters = res$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_two_moons[som_two_moons$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_two_moons)[,3]),
                      mean(silhouette(dist=D,x=dbs_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=res$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)

### aplicação faithful
data_faithful <- scale(faithful)
res_faithful <- SVC(data=data_faithful,sigma_candidates = c(14,15,16.3,17),
                    metric="dunn",scan_n = 20)

par(mfrow=c(1,2))
plot(x=faithful$eruptions,y=faithful$waiting,col = "black",pch = 19,cex = 1.3,
     bty = "n",las = 1,xlab="Erupções",ylab="Tempo de Espera")
grid(col = "gray80", lty = 2)
plot(faithful[,1:2],col=res_faithful$clusters,pch = 19,cex = 1.3,
     xlab = "x",ylab = "y",bty = "n",las = 1)
legend("bottomright",legend=unique(res_faithful$clusters),
       col=unique(res_faithful$clusters),pch=19,cex=1.3,bty='n')
grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_faithful <- kmeans(data_faithful[,1:2], centers = 2)
fcm_faithful <- cmeans(data_faithful[,1:2], centers = 2)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_faithful <- som(as.matrix(data_faithful[,1:2]), grid = som_grid)
som_cluster_faithful <- cutree(hclust(dist(som_faithful$codes[[1]])), k = 2)
hc_faithful <- hclust(dist(data_faithful[,1:2]))
tree_faithful <- cutree(hc_faithful, k = 2)
dbs_faithful <- dbscan(data_faithful[,1:2], eps = 0.3)

par(mfrow = c(2, 3))
plot(faithful[,1:2], pch=19,col=km_faithful$cluster, 
     main="K-means",bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright",legend=sort(unique(km_faithful$cluster)),col=sort(unique(km_faithful$cluster)),pch=19)
grid(col = "gray80", lty = 2)
plot(faithful[,1:2], pch=19, col=fcm_faithful$cluster, 
     main="Fuzzy C-means",bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright", legend=sort(unique(fcm_faithful$cluster)),col=sort(unique(fcm_faithful$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(faithful[,1:2], pch=19, col=som_cluster_faithful[som_faithful$unit.classif], 
     main="SOM",bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright", legend=sort(unique(som_cluster_faithful[som_faithful$unit.classif])),
       col=sort(unique(som_cluster_faithful[som_faithful$unit.classif])), pch=19)
grid(col = "gray80", lty = 2)
plot(faithful[,1:2], pch=19, col=tree_faithful,main="Árvores de Agrupamento",
     bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright", legend=sort(unique(tree_faithful)),col=sort(unique(tree_faithful)), pch=19)
grid(col = "gray80", lty = 2)
plot(faithful[,1:2], pch=19, col=dbs_faithful$cluster, main="DBSCAN",
     bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright", legend=sort(unique(dbs_faithful$cluster)),col=sort(unique(dbs_faithful$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(faithful[,1:2],col=res_faithful$clusters,pch=19,main='SVC',
     bty='n',las=1,cex=1.3,xlab="Erupções",ylab="Tempo de Espera")
legend("bottomright",legend=c(unique(res_faithful$clusters)),col=unique(res_faithful$clusters),pch=19)
grid(col = "gray80", lty = 2)
methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
D <- dist(data_faithful[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_faithful$cluster),
                      dunn(distance = D,clusters = fcm_faithful$cluster),
                      dunn(distance = D,clusters = som_cluster_faithful[som_faithful$unit.classif]),
                      dunn(distance = D,clusters = tree_faithful),
                      dunn(distance = D,clusters = dbs_faithful$cluster),
                      dunn(distance = D,clusters = res_faithful$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_faithful$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_faithful$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_faithful[som_faithful$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_faithful)[,3]),
                      mean(silhouette(dist=D,x=dbs_faithful$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_faithful$clusters)[,3])),3)
data.frame(methods,Dunn_index,Sil_values)

### testes na base seeds
seeds <- read.table("seeds_dataset.txt")
names(seeds) <- c("area","perimetro","compacidade","comprimento_kernel",
                  "largura_kernel","assimetria","comprimento_sulco_kernel")
plot(seeds[,-8],col=seeds[,8],pch=19)

data_seeds <- scale(seeds[,-8])
PCA_seeds <- princomp(data_seeds)
data_pca_seeds <- PCA_seeds$scores[,1:3]
res_seeds <- SVC(data=seeds[,-8],sigma = 2,
                    metric="dunn",scan_n = 20)

par(mfrow=c(1,2))
plot(seeds[,c(1,2)],col = seeds[,8],pch = 19,cex = 1.3,bty = "n",las = 1)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2],col=res_seeds$clusters,pch = 19,cex = 1.3,
     xlab = "x",ylab = "y",bty = "n",las = 1)
legend("bottomright",legend=unique(res_seeds$clusters),
       col=unique(res_seeds$clusters),pch=19,cex=1.3,bty='n')
grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_seeds <- kmeans(data_pca_seeds, centers = 3)
fcm_seeds <- cmeans(data_pca_seeds, centers = 3)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_seeds <- som(as.matrix(data_pca_seeds), grid = som_grid)
som_cluster_seeds <- cutree(hclust(dist(som_seeds$codes[[1]])), k = 3)
hc_seeds <- hclust(dist(data_pca_seeds))
tree_seeds <- cutree(hc_seeds, k = 3)
dbs_seeds <- dbscan(data_pca_seeds, eps = 0.7)

par(mfrow = c(2, 3))
plot(seeds[,1:2], pch=19,col=km_seeds$cluster, 
     main="K-means",bty='n',las=1,cex=1.3)
legend("bottomright",legend=sort(unique(km_seeds$cluster)),col=sort(unique(km_seeds$cluster)),pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2], pch=19, col=fcm_seeds$cluster, 
     main="Fuzzy C-means",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(fcm_seeds$cluster)),col=sort(unique(fcm_seeds$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2], pch=19, col=som_cluster_seeds[som_seeds$unit.classif], 
     main="SOM",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(som_cluster_seeds[som_seeds$unit.classif])),
       col=sort(unique(som_cluster_seeds[som_seeds$unit.classif])), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2], pch=19, col=tree_seeds,main="Árvores de Agrupamento",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(tree_seeds)),col=sort(unique(tree_seeds)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2], pch=19, col=dbs_seeds$cluster, main="DBSCAN",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(dbs_seeds$cluster)),col=sort(unique(dbs_seeds$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(seeds[,1:2],col=res_seeds$clusters,pch=19,main='SVC',
     bty='n',las=1,cex=1.3)
legend("bottomright",legend=c(unique(res_seeds$clusters)),col=unique(res_seeds$clusters),pch=19)
grid(col = "gray80", lty = 2)
methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
D <- dist(data_seeds[,1:2])
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
data.frame(methods,Dunn_index,Sil_values)

### aplicacao no habermans survival
haberman <- read.table("haberman.txt",sep = ",")
names(haberman) <- c("idade","ano_operacao","linfonodos_positivos_axilares","status")
plot(haberman[,1:3],col=haberman[,4],pch=19)
data_haberman <- scale(haberman[,-4])

res_haberman <- SVC(data=data_haberman,sigma=50,
                 metric="dunn",scan_n = 50)

par(mfrow=c(1,2))
plot(haberman[,c(1,3)],col = haberman[,4],pch = 19,cex = 1.3,bty = "n",las = 1)
grid(col = "gray80", lty = 2)
plot(haberman[,c(1,3)],col=res_haberman$clusters,pch = 19,cex = 1.3,
     xlab = "x",ylab = "y",bty = "n",las = 1)
legend("bottomright",legend=unique(res_haberman$clusters),
       col=unique(res_haberman$clusters),pch=19,cex=1.3,bty='n')
grid(col = "gray80", lty = 2)

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_haberman <- kmeans(data_pca_haberman, centers = 3)
fcm_haberman <- cmeans(data_pca_haberman, centers = 3)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_haberman <- som(as.matrix(data_pca_haberman), grid = som_grid)
som_cluster_haberman <- cutree(hclust(dist(som_haberman$codes[[1]])), k = 3)
hc_haberman <- hclust(dist(data_pca_haberman))
tree_haberman <- cutree(hc_haberman, k = 3)
dbs_haberman <- dbscan(data_pca_haberman, eps = 0.7)

par(mfrow = c(2, 3))
plot(haberman[,1:2], pch=19,col=km_haberman$cluster, 
     main="K-means",bty='n',las=1,cex=1.3)
legend("bottomright",legend=sort(unique(km_haberman$cluster)),col=sort(unique(km_haberman$cluster)),pch=19)
grid(col = "gray80", lty = 2)
plot(haberman[,1:2], pch=19, col=fcm_haberman$cluster, 
     main="Fuzzy C-means",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(fcm_haberman$cluster)),col=sort(unique(fcm_haberman$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(haberman[,1:2], pch=19, col=som_cluster_haberman[som_haberman$unit.classif], 
     main="SOM",bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(som_cluster_haberman[som_haberman$unit.classif])),
       col=sort(unique(som_cluster_haberman[som_haberman$unit.classif])), pch=19)
grid(col = "gray80", lty = 2)
plot(haberman[,1:2], pch=19, col=tree_haberman,main="Árvores de Agrupamento",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(tree_haberman)),col=sort(unique(tree_haberman)), pch=19)
grid(col = "gray80", lty = 2)
plot(haberman[,1:2], pch=19, col=dbs_haberman$cluster, main="DBSCAN",
     bty='n',las=1,cex=1.3)
legend("bottomright", legend=sort(unique(dbs_haberman$cluster)),col=sort(unique(dbs_haberman$cluster)), pch=19)
grid(col = "gray80", lty = 2)
plot(haberman[,1:2],col=res_haberman$clusters,pch=19,main='SVC',
     bty='n',las=1,cex=1.3)
legend("bottomright",legend=c(unique(res_haberman$clusters)),col=unique(res_haberman$clusters),pch=19)
grid(col = "gray80", lty = 2)
methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
D <- dist(data_haberman[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_haberman$cluster),
                      dunn(distance = D,clusters = fcm_haberman$cluster),
                      dunn(distance = D,clusters = som_cluster_haberman[som_haberman$unit.classif]),
                      dunn(distance = D,clusters = tree_haberman),
                      dunn(distance = D,clusters = dbs_haberman$cluster),
                      dunn(distance = D,clusters = res_haberman$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_haberman$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_haberman$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_haberman[som_haberman$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_haberman)[,3]),
                      mean(silhouette(dist=D,x=dbs_haberman$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_haberman$clusters)[,3])),3)
data.frame(methods,Dunn_index,Sil_values)

### Aplicacao em clientes
clientes <- read.csv('Wholesale customers data.csv')
plot(clientes[,3:ncol(clientes)],col=clientes$Channel,pch=19)
clientes <- clientes %>% filter(Frozen<30000, Grocery<50000,Delicassen<10000)
head(clientes)
pca_clientes <- prcomp(clientes[,3:8],scale. = TRUE)
round(pca_clientes$sdev/sum(pca_clientes$sdev),3)
plot(pca_clientes$x[,1:2],col=clientes$Channel,pch=19)
plot(pca_clientes$x[,c(1,3)],col=clientes$Channel,pch=19)
require(rgl)
rgl::plot3d(pca_clientes$x[,1],pca_clientes$x[,2],pca_clientes$x[,3],col=clientes$Channel)

data_clientes <- pca_clientes$x[,1:3]

res_clientes <- SVC(data=data_clientes,sigma=2,scan_n=10,kernel="gaussian")
par(mfrow=c(1,2))
# plot(clientes[,3:4],col=clientes$Channel,pch=19,main="Rótulos originais")
# plot(clientes[,3:4],col=res_clientes$clusters,pch=19,main="SVC")
plot(data_clientes[,1:2],col=res_clientes$clusters,pch=19,main="SVC")
plot(data_clientes[,1:2],col=clientes$Channel,pch=19,main="Rótulos originais")

