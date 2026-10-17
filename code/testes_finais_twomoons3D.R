### teste twomoons 3D
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
library(rgl)
options(device='x11')
two_moons <- read.csv('moonDataset.csv')
# rgl::plot3d(x=two_moons[,1],
#             y=two_moons[,2],
#             z=two_moons[,3],
#             col=two_moons[,4]+1)
# plot(two_moons[,2:3],col=two_moons[,4]+1)
# view_angle <- list(theta = -15, phi = 15, zoom = 1.3)
# rgl.open()
# par3d(windowRect = c(0, 0, 400, 400))
# plot3d(x = two_moons[, 1],y = two_moons[, 2],z = two_moons[, 3],
#        col = two_moons[,4]+1,size = 4,xlab = "x", ylab = "y", zlab = "z")
# rgl.viewpoint(theta = view_angle$theta, phi = view_angle$phi, zoom = view_angle$zoom)
# legend3d("topright",legend = levels(factor(two_moons[,4]+1)),col = unique(two_moons[,4]+1),pch = 19,cex=1.5)
# rgl.snapshot("two_moons3D.png", fmt = "png")

### clusterização SVC
res_two_moons <- SVC(data=two_moons[,1:3],n_sigma_search = 10,scan_n = 50)
# par(mfrow=c(1,2))
# plot(two_moons[,1], two_moons$y,pch = 16,cex = 0.7,xlim=c(-2,12),
#      col= two_moons$CL,xlab="X1",ylab="X2",main="Rótulos originais")
# legend("bottomright",legend=unique(two_moons$CL),col=unique(two_moons$CL),pch=16)
# plot(two_moons[,1], two_moons$y,pch = 16,cex = 0.7,xlim=c(-2,12),
#      col= res_two_moons$clusters,xlab="X1",ylab="X2",main="SVC")
# legend("bottomright",legend=unique(res_two_moons$clusters),col=unique(res_two_moons$clusters),pch=16)
### aplicando DBSCAN, Kmeans, F Cmeans, SOM e Árvores de Clusterizacao

set.seed(123)
km_two_moons <- kmeans(two_moons[,1:3], centers = 2)
fcm_two_moons <- cmeans(two_moons[,1:3], centers = 2)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_two_moons <- som(as.matrix(two_moons[,1:3]), grid = som_grid)
som_cluster_two_moons <- cutree(hclust(dist(som_two_moons$codes[[1]])), k = 2)
hc_two_moons <- hclust(dist(two_moons[,1:3]))
tree_two_moons <- cutree(hc_two_moons, k = 2)
dbs_two_moons <- dbscan(two_moons[,1:3], eps = 0.23)

par(mfrow = c(2, 3))
plot(two_moons[,1:2], pch=19,col=km_two_moons$cluster, main="K-means",bty='n',las=1)
legend("bottomright",legend=sort(unique(km_two_moons$cluster)),col=sort(unique(km_two_moons$cluster)),pch=19)
plot(two_moons[,1:2], pch=19,col=fcm_two_moons$cluster, main="Fuzzy C-means",bty='n',las=1)
legend("bottomright",legend=sort(unique(fcm_two_moons$cluster)),col=sort(unique(fcm_two_moons$cluster)),pch=19)
plot(two_moons[,1:2], pch=19,col=som_cluster_two_moons[som_two_moons$unit.classif], main="SOM",bty='n',las=1)
legend("bottomright",legend=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])),
       col=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])),pch=19)
plot(two_moons[,1:2], pch=19,col=tree_two_moons, main="Árvores de agrupamento",bty='n',las=1)
legend("bottomright",legend=sort(unique(tree_two_moons)),col=sort(unique(tree_two_moons)),pch=19)
plot(two_moons[,1:2], pch=19,col=dbs_two_moons$cluster+1, main="DBSCAN",bty='n',las=1)
legend("bottomright",legend=sort(unique(dbs_two_moons$cluster)+1),col=sort(unique(dbs_two_moons$cluster)+1),pch=19)
plot(two_moons[,1:2],col=res_two_moons$clusters,pch=19,main="SVC",bty='n',las=1)
legend("bottomright",legend=c(unique(res_two_moons$clusters)),col=unique(res_two_moons$clusters),pch=19)

# par(mfrow = c(3, 2))
# plot(two_moons[,2:3], pch=19,col=km_two_moons$cluster, main="K-means",bty='n',las=1)
# legend("bottomright",legend=sort(unique(km_two_moons$cluster)),col=sort(unique(km_two_moons$cluster)),pch=19)
# plot(two_moons[,2:3], pch=19,col=fcm_two_moons$cluster, main="Fuzzy C-means",bty='n',las=1)
# legend("bottomright",legend=sort(unique(fcm_two_moons$cluster)),col=sort(unique(fcm_two_moons$cluster)),pch=19)
# plot(two_moons[,2:3], pch=19,col=som_cluster_two_moons[som_two_moons$unit.classif], main="SOM",bty='n',las=1)
# legend("bottomright",legend=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])),
#        col=sort(unique(som_cluster_two_moons[som_two_moons$unit.classif])),pch=19)
# plot(two_moons[,2:3], pch=19,col=tree_two_moons, main="Árvores de agrupamento",bty='n',las=1)
# legend("bottomright",legend=sort(unique(tree_two_moons)),col=sort(unique(tree_two_moons)),pch=19)
# plot(two_moons[,2:3], pch=19,col=dbs_two_moons$cluster+1, main="DBSCAN",bty='n',las=1)
# legend("bottomright",legend=sort(unique(dbs_two_moons$cluster)+1),col=sort(unique(dbs_two_moons$cluster)+1),pch=19)
# plot(two_moons[,2:3],col=res_two_moons$clusters,pch=19,main="SVC",bty='n',las=1)
# legend("bottomright",legend=c(unique(res_two_moons$clusters)),col=unique(res_two_moons$clusters),pch=19)

# view_angle <- list(theta = -15, phi = 15, zoom = 1.3)
# rgl.open()
# par3d(windowRect = c(0, 0, 400, 400))
# plot3d(x = two_moons[, 1],y = two_moons[, 2],z = two_moons[, 3],
#   col = res_two_moons$clusters,size = 4,xlab = "x", ylab = "y", zlab = "z")
# rgl.viewpoint(theta = view_angle$theta, phi = view_angle$phi, zoom = view_angle$zoom)
# legend3d("topright",legend = levels(factor(res_two_moons$clusters)),col = unique(res_two_moons$clusters),pch = 19,cex=1.5)
# rgl.snapshot("SVC_two_moons3D.png", fmt = "png")

# rgl.open()
# par3d(windowRect = c(0, 0, 400, 400))
# plot3d(x = two_moons[, 1],y = two_moons[, 2],z = two_moons[, 3],
#   col = dbs_two_moons$cluster + 1,size = 4,xlab = "x", ylab = "y", zlab = "z")
# rgl.viewpoint(theta = view_angle$theta, phi = view_angle$phi, zoom = view_angle$zoom)
# legend3d("topright",legend = sort(unique(dbs_two_moons$cluster + 1)),col = sort(unique(dbs_two_moons$cluster + 1)),pch = 19,cex=1.5)
# rgl.snapshot("DBSCAN_two_moons3D.png", fmt = "png")

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
y <- two_moons[,4]
ARI <- c(
  adjustedRandIndex(km_two_moons$cluster, y),
  adjustedRandIndex(fcm_two_moons$cluster, y),
  adjustedRandIndex(som_cluster_two_moons[som_two_moons$unit.classif], y),
  adjustedRandIndex(tree_two_moons, y),
  adjustedRandIndex(dbs_two_moons$cluster, y),
  adjustedRandIndex(res_two_moons$clusters, y))
D <- dist(two_moons[,1:3])
Dunn_index <- round(c(dunn(distance = D,clusters = km_two_moons$cluster),
                      dunn(distance = D,clusters = fcm_two_moons$cluster),
                      dunn(distance = D,clusters = som_cluster_two_moons[som_two_moons$unit.classif]),
                      dunn(distance = D,clusters = tree_two_moons),
                      dunn(distance = D,clusters = dbs_two_moons$cluster),
                      dunn(distance = D,clusters = res_two_moons$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_two_moons[som_two_moons$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_two_moons)[,3]),
                      mean(silhouette(dist=D,x=dbs_two_moons$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_two_moons$clusters)[,3])),3)
data.frame(methods,ARI,Dunn_index,Sil_values)