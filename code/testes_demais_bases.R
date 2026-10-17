### testes outras bases reais
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
library(readxl)
options(device='x11')
user_knowledge <- readxl::read_xls("user_knowledge.xls",
                                   "Training_Data",
                                   range="A1:F259")
# str(user_knowledge)
y_uknowledge <- factor(user_knowledge$UNS)
# plot(user_knowledge[,1:4],col=y_uknowledge)

gallstone <- readxl::read_excel("gallstone.xlsx")
# str(gallstone)
gallstone_cluster <- gallstone[,-c(1:8)]
# plot(gallstone_cluster[,9:ncol(gallstone_cluster)],col=gallstone$`Gallstone Status`+1)

nomes_wisconsin <- c('ID','Diagnosis','radius1','texture1',
                     'perimeter1','area1','smoothness1','compactness1',
                     'concavity1', 'concave_points1','symmetry1',
                     'fractal_dimension1','radius2','texture2','perimeter2',
                     'area2','smoothness2','compactness2','concavity2','concave_points2',
                     'symmetry2','fractal_dimension2','radius3','texture3',
                     'perimeter3','area3','smoothness3','compactness3',
                     'concavity3','concave_points3','symmetry3','fractal_dimension3')
wisconsin_cancer <- read.csv("wisconsin_breast_cancer.csv",col.names = nomes_wisconsin)
wisconsin_cluster <- wisconsin_cancer[,-c(1,2)]
# str(wisconsin_cluster)


absenteeism_at_work <- read_excel("absenteeism_at_work.xlsx")
absenteeism_cluster <- absenteeism_at_work[,-c(1,2,3,4,5,21)]
# str(absenteeism_cluster)
# plot(absenteeism_cluster)

hcv_data <- read.csv("hcv_data.csv")
y_hcv <- factor(hcv_data$Category)
# str(hcv_data)
hcv_clust <- hcv_data[,-c(1,2,3,4)]
# plot(hcv_clust,col=y_hcv)

heart_failure <- read.csv("heart_failure_data.csv")
# str(heart_failure)
heart_cluster <- heart_failure[,-13]
# plot(heart_cluster,col=factor(heart_failure$DEATH_EVENT))

cervical_cancer <- read.csv("cervical_cancer_behavior.csv")
y_cervcancer <- cervical_cancer$ca_cervix+1
# str(cervical_cancer)
cervcancer_cluster <- cervical_cancer[,-20]
# plot(cervcancer_cluster[,16:19],col=y_cervcancer)




res_userknow <- SVC(data=scale(user_knowledge[,-6]),sigma=2,
                    scan_n = 20,metric='dunn')
res_gallstone <- SVC(data=scale(gallstone_cluster),sigma=0.05,scan_n = 10)
# res_hcv <- SVC(data=scale(hcv_clust),sigma=10,scan_n=10)
res_wisconsin <- SVC(data=scale(wisconsin_cluster),sigma=0.1,scan_n = 5)
res_absenteeism <- SVC(data=scale(absenteeism_cluster),sigma=0.5,scan_n=5)
res_heart <- SVC(data=scale(heart_cluster),n_sigma_search = 10,scan_n = 30)
res_cervical <- SVC(data=scale(cervcancer_cluster),n_sigma_search=20,scan_n = 50)

par(mfrow=c(3,2))
plot(user_knowledge[,1:2],col=res_userknow$clusters,pch=19,cex=1.3,main="User knowledge")
legend("topright",legend=unique(res_userknow$clusters),col=unique(res_userknow$clusters),pch=19)

plot(gallstone_cluster[,1:2],col=res_gallstone$clusters,pch=19,cex=1.3,main="Gallstone")
legend("topright",legend=unique(res_gallstone$clusters),col=unique(res_gallstone$clusters),pch=19)

# plot(hcv_clust[,1:2],col=res_hcv$clusters,pch=19,cex=1.3,main="HCV")
# legend("topright",legend=unique(res_hcv$clusters),col=unique(res_hcv$clusters),pch=19)

plot(wisconsin_cluster[,1:2],col=res_wisconsin$clusters,pch=19,cex=1.3,main="Wisconsin")
legend("topright",legend=unique(res_wisconsin$clusters),col=unique(res_wisconsin$clusters),pch=19)

plot(absenteeism_cluster[,1:4],col=res_absenteeism$clusters,pch=19,cex=1.3,main="Absenteeism")
legend("topright",legend=unique(res_absenteeism$clusters),col=unique(res_absenteeism$clusters),pch=19)

plot(heart_cluster[,1:2],col=res_heart$clusters,pch=19,cex=1.3,main="Heart disease")
legend("topright",legend=unique(res_heart$clusters),col=unique(res_heart$clusters),pch=19)

plot(cervcancer_cluster[,1:2],col=res_cervical$clusters,pch=19,cex=1.3,main="Cervical Cancer")
legend("topright",legend=unique(res_cervical$clusters),col=unique(res_cervical$clusters),pch=19)


library(purrr)
library(tidyverse)
library(magick)
library(gridExtra)
### aplicacao walking
setwd("C:/Users/lucas/OneDrive/Área de Trabalho/Estudo/UFPR/TCC/rcode/User Identification From Walking Activity")
nomes_walking <- c('time', 'x', 'y', 'z')
dados_combinados <- map_dfr(
  .x = sprintf("%d.csv", 1:22),
  .f = ~ read_csv(.x, col_names = nomes_walking) %>%  # Atribui os nomes das colunas
    mutate(ID = as.integer(gsub("\\.csv$", "", .x))),  # Adiciona coluna ID
  .id = NULL
)
glimpse(dados_combinados)

resumo_usuarios <- dados_combinados %>%
  group_by(ID) %>%
  summarise(
    media_x = mean(x, na.rm = TRUE),media_y = mean(y, na.rm = TRUE),media_z = mean(z, na.rm = TRUE),
    duracao = max(time) - min(time))
head(resumo_usuarios)
walking_clust <- resumo_usuarios[,-c(1,5)]

### SVC
res_walking <- SVC(data=scale(walking_clust),sigma=1.5,
                   scan_n=100,metric="silhouette") ### scan=100 e sigma=2 deu certo
### sigma 1.5 e scan = 100 deu certo também
plot(walking_clust,col=res_walking$clusters,pch=19)
legend("topright",legend=unique(res_walking$clusters),col=unique(res_walking$clusters),pch=19)

wss <- sapply(1:10, function(k) {
  kmeans(scale(walking_clust), k, nstart = 25)$tot.withinss
})
plot(1:10, wss,type = "b",pch = 19,frame = FALSE,
     xlab = "Número de clusters K",ylab = "Soma total dos quadrados intra-cluster (WSS)",main = "Método do Cotovelo para K ótimo")
k <- 5
kNNdistplot(scale(walking_clust), k = k)
abline(h = 1.4, col = "red")

set.seed(123)
km_walking <- kmeans(walking_clust, centers = 4)
fcm_walking <- cmeans(walking_clust, centers = 4)
som_grid <- somgrid(xdim = 3, ydim = 3, topo = "hexagonal")
som_walking <- som(as.matrix(walking_clust), grid = som_grid)
som_cluster_walking <- cutree(hclust(dist(som_walking$codes[[1]])), k = 4)
hc_walking <- hclust(dist(walking_clust))
tree_walking <- cutree(hc_walking, k = 4)
dbs_walking <- dbscan(scale(walking_clust), eps = 1.4,minPts = 5)

par(mfrow = c(2, 3))
plot(walking_clust[,c(1,3)],cex=1.5 ,pch=19,col=km_walking$cluster, main="K-means",bty='n',las=1)
legend("topright",legend=sort(unique(km_walking$cluster)),col=sort(unique(km_walking$cluster)),pch=19)
plot(walking_clust[,c(1,3)],cex=1.5, pch=19,col=fcm_walking$cluster, main="Fuzzy C-means",bty='n',las=1)
legend("topright",legend=sort(unique(fcm_walking$cluster)),col=sort(unique(fcm_walking$cluster)),pch=19)
plot(walking_clust[,c(1,3)],cex=1.5, pch=19,col=som_cluster_walking[som_walking$unit.classif], main="SOM",bty='n',las=1)
legend("topright",legend=sort(unique(som_cluster_walking[som_walking$unit.classif])),
       col=sort(unique(som_cluster_walking[som_walking$unit.classif])),pch=19)
plot(walking_clust[,c(1,3)],cex=1.5, pch=19,col=tree_walking, main="Árvores de agrupamento",bty='n',las=1)
legend("topright",legend=sort(unique(tree_walking)),col=sort(unique(tree_walking)),pch=19)
plot(walking_clust[,c(1,3)],cex=1.5, pch=19,col=dbs_walking$cluster+1, main="DBSCAN",bty='n',las=1)
legend("topright",legend=sort(unique(dbs_walking$cluster)+1),col=sort(unique(dbs_walking$cluster)+1),pch=19)
plot(walking_clust[,c(1,3)],cex=1.5,col=res_walking$clusters,pch=19,main="SVC",bty='n',las=1)
legend("topright",legend=c(unique(res_walking$clusters)),col=unique(res_walking$clusters),pch=19)


methods <- c("K-means","Fuzzy C-means","SOM","Árvores","DBSCAN","SVC")
D <- dist(walking_clust)
Dunn_index <- round(c(dunn(distance = D,clusters = km_walking$cluster),
                      dunn(distance = D,clusters = fcm_walking$cluster),
                      dunn(distance = D,clusters = som_cluster_walking[som_walking$unit.classif]),
                      dunn(distance = D,clusters = tree_walking),
                      dunn(distance = D,clusters = dbs_walking$cluster+1),
                      dunn(distance = D,clusters = res_walking$clusters)),3)
Sil_values <- round(c(mean(silhouette(dist=D,x=km_walking$cluster)[,3]),
                      mean(silhouette(dist=D,x=fcm_walking$cluster)[,3]),
                      mean(silhouette(dist=D,x=som_cluster_walking[som_walking$unit.classif]+1)[,3]),
                      mean(silhouette(dist=D,x=tree_walking)[,3]),
                      mean(silhouette(dist=D,x=dbs_walking$cluster)[,3]),
                      mean(silhouette(dist=D,x=res_walking$clusters)[,3])),3)
data.frame(methods,Dunn_index,Sil_values)

walking_clust <- data.frame(walking_clust)
setwd("C:/Users/lucas/OneDrive/Área de Trabalho/Estudo/UFPR/TCC/rcode")

open3d()
plot3d(walking_clust, col=km_walking$cluster, size=5,main="K-means",
       xlab="x", ylab="y", zlab="z")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("kmeans_walk.png")
rgl.close()

open3d()
plot3d(walking_clust,col=fcm_walking$cluster,main="Fuzzy C-means",
       size=5,xlab="x",ylab="y",zlab = "z")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("fcmeans_walk.png")
rgl.close()

open3d()
plot3d(walking_clust,col=som_cluster_walking[som_walking$unit.classif],
       size=5,xlab="x",ylab="y",zlab = "z",main="SOM")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("SOM_walk.png")
rgl.close()

open3d()
plot3d(walking_clust,col=tree_walking,main="Arvores de agrupamento",
       size=5,xlab="x",ylab="y",zlab = "z")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("arvores_walk.png")
rgl.close()

open3d()
plot3d(walking_clust,col=dbs_walking$cluster+1,size=5,
       xlab="x",ylab="y",zlab = "z",main="DBSCAN")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("DBSCAN_walk.png")
rgl.close()

open3d()
plot3d(walking_clust,col=res_walking$clusters,size=5,xlab="x",ylab="y",zlab = "z",main="SVC")
rgl.viewpoint(theta = 5, phi = 5)
rgl.snapshot("SVC_walk.png")
rgl.close()

images <- lapply(c("kmeans_walk.png", "fcmeans_walk.png", "SOM_walk.png", 
                   "arvores_walk.png", "DBSCAN_walk.png", "SVC_walk.png"), function(x) {
                     img <- image_read(x)
                     ggplot() + annotation_custom(grid::rasterGrob(img)) + theme_void()
                   })

grid.arrange(grobs = images, nrow = 2, ncol = 3)
