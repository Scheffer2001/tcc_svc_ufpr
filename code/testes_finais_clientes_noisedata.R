### testes wholesale customers e noise data
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
### Wholesale
dados <- read.csv("https://raw.githubusercontent.com/TrainingByPackt/Data-Science-with-Python/refs/heads/master/Chapter01/Data/Wholesale%20customers%20data.csv")
plot(dados[,3:8],col=dados$Channel,pch=19)

### noise data
set.seed(01)
n1 <- 200
X <- runif(n1,0,4)+rnorm(n1, mean = 0, sd = 0.1)
Y <- 5.5+X+rnorm(n1,0,0.70)
cluster1_data <- data.frame(x = X[X>0 & Y<10], y = Y[X>0 & Y<10],CL=1)
n2 <- 200
X <- runif(n2,2,6)+rnorm(n2, mean = 0, sd = 0.5)
Y <- 2-sin(X)+rnorm(n2,0,0.30)
cluster2_data <- data.frame(x = X-1+1, y = Y,CL=2)
n3 <- 200
X <- runif(n3,3,7)+rnorm(n3, mean = 0, sd = 0.5)
Y <- 0.4*X-cos(X)+rnorm(n3,0,0.15)
cluster3_data <- data.frame(x = X-.5+1, y = Y+3,CL=3)
n4 <- 200
X <- rnorm(n4, mean = 7, sd = 0.5)
Y <- rnorm(n4,2.5,0.5)
cluster4_data <- data.frame(x = X[X>6.5]-0.5+1, y = Y[X>6.5],CL=4)
n5 <- 200
X <- rnorm(n5, mean = 8, sd = 0.15)
Y <- runif(n5,4,10)
cluster5_data <- data.frame(x = X+1, y = Y,CL="gold")
n_n <- 200
X <- runif(n_n, min = 0, max = 10) 
Y <- runif(n_n, min = 0, max = 10) 
noise_data <- data.frame(x = X, y = Y,CL="gray")


dados_gen <- rbind(cluster1_data,cluster2_data,cluster3_data,
                   cluster4_data,cluster5_data,noise_data)
plot(dados_gen[,1:2],col=dados_gen[,3],pch=19)