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
### Noisy data clustering
set.seed(01)
n1 <- 50
X <- runif(n1,0,3)+rnorm(n1, mean = 0, sd = 0.1)
Y <- 5.5+X+rnorm(n1,0,0.70)
cluster1_data <- data.frame(x = X[X>0 & Y<10], y = Y[X>0 & Y<10],CL=1)
n2 <- 50
X <- runif(n2,2,6)+rnorm(n2, mean = 0, sd = 0.5)
Y <- 2-sin(X)+rnorm(n2,0,0.30)
cluster2_data <- data.frame(x = X-1+1, y = Y,CL=2)
n3 <- 50
X <- runif(n3,3,7)+rnorm(n3, mean = 0, sd = 0.5)
Y <- 0.4*X-cos(X)+rnorm(n3,0,0.15)
cluster3_data <- data.frame(x = X-.5+1, y = Y+3,CL=3)
# n4 <- 50
# X <- rnorm(n4, mean = 7, sd = 0.5)
# Y <- rnorm(n4,2.5,0.5)
# cluster4_data <- data.frame(x = X[X>6.5]-0.5+1, y = Y[X>6.5],CL=4)
n5 <- 50
X <- rnorm(n5, mean = 8, sd = 0.15)
Y <- runif(n5,4,10)
cluster5_data <- data.frame(x = X+1, y = Y,CL="gold")
n_n <- 50
X <- runif(n_n, min = 0, max = 10) 
Y <- runif(n_n, min = 0, max = 10) 
noise_data <- data.frame(x = X, y = Y,CL="gray")
# dados_noisy <- rbind(cluster1_data,cluster2_data,cluster3_data,
#                    cluster4_data,cluster5_data,noise_data)
# dados_gen <- rbind(cluster1_data,cluster2_data,cluster3_data,
#                    cluster4_data,cluster5_data)
dados_noisy <- rbind(cluster1_data,cluster2_data,cluster3_data,
                     cluster5_data,noise_data)
dados_gen <- rbind(cluster1_data,cluster2_data,cluster3_data,
                   cluster5_data)
dados_gen$CL <- ifelse(dados_gen$CL=="gold",4,dados_gen$CL)
par(mfrow=c(1,2))
plot(dados_gen$x, dados_gen$y,pch = 16,bty='n',cex = 0.7,xlim=c(0,10),col= dados_gen$CL,xlab="X1",ylab="X2")
legend("bottomright",legend=unique(dados_gen$CL),col=unique(dados_gen$CL),pch=16)
plot(dados_noisy$x, dados_noisy$y,pch = 16,cex = 0.7,xlim=c(0,10),col= dados_noisy$CL,xlab="X1",ylab="X2")
### clusterização SVC
res_noisy <- SVC(data=dados_gen[,1:2],sigma=2,scan_n = 10)
# par(mfrow=c(1,2))
# plot(dados_gen$x, dados_gen$y,pch = 16,cex = 0.7,xlim=c(-2,12),
#      col= dados_gen$CL,xlab="X1",ylab="X2",main="Rótulos originais")
# legend("bottomright",legend=unique(dados_gen$CL),col=unique(dados_gen$CL),pch=16)
# plot(dados_gen$x, dados_gen$y,pch = 16,cex = 0.7,xlim=c(-2,12),
#      col= res_noisy$clusters,xlab="X1",ylab="X2",main="SVC")
# legend("bottomright",legend=unique(res_noisy$clusters),col=unique(res_noisy$clusters),pch=16)
### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_gen <- kmeans(dados_gen[,1:2], centers = 4)
fcm_gen <- cmeans(dados_gen[,1:2], centers = 4)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_gen <- som(as.matrix(dados_gen[,1:2]), grid = som_grid)
som_cluster_gen <- cutree(hclust(dist(som_gen$codes[[1]])), k = 4)
hc_gen <- hclust(dist(dados_gen[,1:2]))
tree_gen <- cutree(hc_gen, k = 4)
dbs_gen <- dbscan(dados_gen[,1:2], eps = 0.75)

par(mfrow = c(2, 3))
plot(dados_gen[,1:2], pch=19,col=km_gen$cluster, main="K-means",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(km_gen$cluster)),col=sort(unique(km_gen$cluster)),pch=19)
plot(dados_gen[,1:2], pch=19,col=fcm_gen$cluster, main="Fuzzy C-means",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(fcm_gen$cluster)),col=sort(unique(fcm_gen$cluster)),pch=19)
plot(dados_gen[,1:2], pch=19,col=som_cluster_gen[som_gen$unit.classif], main="SOM",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(som_cluster_gen[som_gen$unit.classif])),
       col=sort(unique(som_cluster_gen[som_gen$unit.classif])),pch=19)
plot(dados_gen[,1:2], pch=19,col=tree_gen, main="Árvores de agrupamento",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(tree_gen)),col=sort(unique(tree_gen)),pch=19)
plot(dados_gen[,1:2], pch=19,col=dbs_gen$cluster+1, main="DBSCAN",bty='n',xlim=c(-2,12),las=1)
legend("bottomright",legend=sort(unique(dbs_gen$cluster)+1),col=sort(unique(dbs_gen$cluster)+1),pch=19)
plot(dados_gen[,1:2],col=res_noisy$clusters,pch=19,main="SVC ",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=c(unique(res_noisy$clusters)),col=unique(res_noisy$clusters),pch=19)

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- dados_gen$CL
ARI <- c(
  adjustedRandIndex(km_gen$cluster, y),
  adjustedRandIndex(fcm_gen$cluster, y),
  adjustedRandIndex(som_cluster_gen[som_gen$unit.classif], y),
  adjustedRandIndex(tree_gen, y),
  adjustedRandIndex(dbs_gen$cluster, y),
  adjustedRandIndex(res_noisy$clusters, y))
D <- dist(dados_gen[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_gen$cluster),
                      dunn(distance = D,clusters = fcm_gen$cluster),
                      dunn(distance = D,clusters = som_cluster_gen[som_gen$unit.classif]),
                      dunn(distance = D,clusters = tree_gen),
                      dunn(distance = D,clusters = dbs_gen$cluster),
                      dunn(distance = D,clusters = res_noisy$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_gen$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_gen$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_gen[som_gen$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_gen)[,3]),
                      mean(silhouette(dist=D,x=dbs_gen$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_noisy$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)


res_noisy2 <- SVC(data=dados_noisy[,1:2],sigma=2,scan_n = 50)
par(mfrow=c(1,2))
plot(dados_noisy$x, dados_noisy$y,pch = 16,cex = 0.7,xlim=c(0,10),
     col= dados_noisy$CL,xlab="X1",ylab="X2")
plot(dados_noisy$x, dados_noisy$y,pch = 16,cex = 0.7,xlim=c(0,10),
     col= res_noisy2$clusters,xlab="X1",ylab="X2")

### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_noisy <- kmeans(dados_noisy[,1:2], centers = 5)
fcm_noisy <- cmeans(dados_noisy[,1:2], centers = 5)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_noisy <- som(as.matrix(dados_noisy[,1:2]), grid = som_grid)
som_cluster_noisy <- cutree(hclust(dist(som_noisy$codes[[1]])), k = 5)
hc_noisy <- hclust(dist(dados_noisy[,1:2]))
tree_noisy <- cutree(hc_noisy, k = 5)
dbs_noisy <- dbscan(dados_noisy[,1:2], eps = 0.5)

par(mfrow = c(2, 3))
plot(dados_noisy[,1:2], pch=19,col=km_noisy$cluster, main="K-means",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(km_noisy$cluster)),col=sort(unique(km_noisy$cluster)),pch=19)
plot(dados_noisy[,1:2], pch=19,col=fcm_noisy$cluster, main="Fuzzy C-means",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(fcm_noisy$cluster)),col=sort(unique(fcm_noisy$cluster)),pch=19)
plot(dados_noisy[,1:2], pch=19,col=som_cluster_noisy[som_noisy$unit.classif], main="SOM",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(som_cluster_noisy[som_noisy$unit.classif])),
       col=sort(unique(som_cluster_noisy[som_noisy$unit.classif])),pch=19)
plot(dados_noisy[,1:2], pch=19,col=tree_noisy, main="Árvores de agrupamento",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(tree_noisy)),col=sort(unique(tree_noisy)),pch=19)
plot(dados_noisy[,1:2], pch=19,col=dbs_noisy$cluster+1, main="DBSCAN",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=sort(unique(dbs_noisy$cluster)+1),col=sort(unique(dbs_noisy$cluster)+1),pch=19)
plot(dados_noisy[,1:2],col=res_noisy2$clusters,pch=19,main="SVC",xlim=c(-2,12),bty='n',las=1)
legend("bottomright",legend=c(unique(res_noisy2$clusters)),col=unique(res_noisy2$clusters),pch=19)

y <- dados_noisy$CL
ARI <- c(
  adjustedRandIndex(km_noisy$cluster, y),
  adjustedRandIndex(fcm_noisy$cluster, y),
  adjustedRandIndex(som_cluster_noisy[som_noisy$unit.classif], y),
  adjustedRandIndex(tree_noisy, y),
  adjustedRandIndex(dbs_noisy$cluster, y),
  adjustedRandIndex(res_noisy2$clusters, y))
D <- dist(dados_noisy[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_noisy$cluster),
                      dunn(distance = D,clusters = fcm_noisy$cluster),
                      dunn(distance = D,clusters = som_cluster_noisy[som_noisy$unit.classif]),
                      dunn(distance = D,clusters = tree_noisy),
                      dunn(distance = D,clusters = dbs_noisy$cluster),
                      dunn(distance = D,clusters = res_noisy2$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_noisy$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_noisy$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_noisy[som_noisy$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_noisy)[,3]),
                      mean(silhouette(dist=D,x=dbs_noisy$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_noisy2$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)
