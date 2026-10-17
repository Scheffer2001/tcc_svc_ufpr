setwd('C:/Users/lucas/OneDrive/Área de Trabalho/Estudo/UFPR/TCC/rcode')
library(SVcluster)
library(class)
library(dbscan)
library(kohonen)
library(mclust)
library(e1071)
library(clValid)
library(Rtsne)
library(cluster)
library(ggplot2)
require(dplyr)
require(factoextra)
require(maps)
require(mapdata)
options(device='x11')
URL = "https://raw.githubusercontent.com/"
pathGithub = "andersonara/datasets/master/"
arquivo = "capitais_br.csv"
dados <- read.csv(paste0(URL,pathGithub,arquivo),
                  header=T,dec=',',sep=';',encoding = "latin1")
variaveis <- dados %>% select(lat, lng) %>% scale() %>% data.frame()
res_capitais <- SVC(variaveis,scan_n=10,metric="dunn",
                    sigma_candidates = seq(0.5,10,by=0.1))

# par(mar=c(1,1,1,1))
# map("worldHires","Brazil")
# points(dados$lng,dados$lat,pch=19,cex=2)
# 
# # par(mar=c(1,1,1,1))
# map("worldHires","Brazil")
# points(dados$lng,dados$lat,col=res_capitais$clusters,pch=19,cex=2)
# 
# 
set.seed(123)
km_capitais <- kmeans(variaveis[,1:2], centers = 5)
fcm_capitais <- cmeans(variaveis[,1:2], centers = 5)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_capitais <- som(as.matrix(variaveis[,1:2]), grid = som_grid)
som_cluster_capitais <- cutree(hclust(dist(som_capitais$codes[[1]])), k = 5)
hc_capitais <- hclust(dist(variaveis[,1:2]))
tree_capitais <- cutree(hc_capitais, k = 5)
dbs_capitais <- dbscan(variaveis[,1:2], eps = 0.75)

par(mfrow = c(2, 3), mar = c(0.5, 0.5, 3, 0.5),mgp = c(1.5, 0.5, 0),cex.main = 1.2)
map("worldHires","Brazil")
points(dados$lng,dados$lat,col=km_capitais$cluster,pch=19,cex=2)
title("K-means")

map("worldHires","Brazil")
points(dados$lng,dados$lat,col=fcm_capitais$cluster,pch=19,cex=2)
title("Fuzzy C-means")

map("worldHires","Brazil")
points(dados$lng,dados$lat,col=som_cluster_capitais[som_capitais$unit.classif],pch=19,cex=2)
title("Self-Organizing Map")

map("worldHires","Brazil")
points(dados$lng,dados$lat,col=tree_capitais,pch=19,cex=2)
title("Árvores de agrupamento")

map("worldHires","Brazil")
points(dados$lng,dados$lat,col=dbs_capitais$cluster+1,pch=19,cex=2)
title("DBSCAN")

map("worldHires","Brazil")
points(dados$lng,dados$lat,col=res_capitais$clusters,pch=19,cex=2)
title("SVC")

methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
D <- dist(variaveis[,1:2])
Dunn_index <- round(c(dunn(distance = D,clusters = km_capitais$cluster),
                      dunn(distance = D,clusters = fcm_capitais$cluster),
                      dunn(distance = D,clusters = som_cluster_capitais[som_capitais$unit.classif]),
                      dunn(distance = D,clusters = tree_capitais),
                      dunn(distance = D,clusters = dbs_capitais$cluster),
                      dunn(distance = D,clusters = res_capitais$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_capitais$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_capitais$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_capitais[som_capitais$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_capitais)[,3]),
                      mean(silhouette(dist=D,x=dbs_capitais$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_capitais$clusters)[,3])),3)
data.frame(methods,Dunn_index,Sil_values)
