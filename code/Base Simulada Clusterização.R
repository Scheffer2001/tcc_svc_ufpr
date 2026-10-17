library(factoextra)
library(gridExtra)
library(ggplot2)
require(dplyr)
require(igraph)
require(quadprog)
library(clValid)
library(mclust)
library(cluster)
library(Matrix)
set.seed(2025)
options(device = 'x11')
n <- 200
Feature1 <- rnorm(n)
Feature2_0 <- Feature1 + rnorm(n, sd = 0.5)  # Correlação positiva para Classe 0
Feature2_1 <- -Feature1 + rnorm(n, sd = 0.5) # Correlação negativa para Classe 1
labels <- sample(0:1, n, replace = TRUE)

df <- data.frame(
  Feature1 = Feature1,
  Feature2 = ifelse(labels == 0, Feature2_0, Feature2_1),
  Classe = as.factor(labels)
)

X = df[1:2]
SVC <- function(data,sigma,varredura){
  start <- Sys.time()
  n = nrow(X)
  kernel_func <- function(sigma,x, y) {
    return(exp(-sigma * sum((x - y)^2)))
  }
  # matriz do kernel, onde kij é a similaridade entre i e j
  Kij <- t(apply(X, 1, function(p)
    apply(X, 1, function(q)
      kernel_func(sigma,p, q))))
  Kjj <- diag(Kij)
  
  Dmat = Kij
  dvec = Kjj
  
  Dmat <- nearPD(Dmat)
  Dmat <- as.matrix(Dmat$mat)  
  autovalores <- eigen(Dmat)$values
  if (any(autovalores <= 0)) {
    delta <- 1e-10
    Dmat <- Dmat + diag(delta, nrow(Dmat))
  }
  
  sc <- norm(Dmat,'2')
  # delta <- 1e-10
  # Dmat <- Dmat + diag(delta, nrow(Dmat))
  # autovalores<- eigen(Dmat)
  
  A.l1 <-  rep(1, n)  #sum>=1
  A.l2 <- -rep(1, n)  #sum<=1
  A.p1  <-   diag(n)   #betaj>=0
  A.p2  <-  -diag(n)   #betaj<=C
  Amat = t(rbind(A.l1, A.l2, A.p1, A.p2))
  
  b0 <- c(1, -1, rep(0, n), rep(-1, n))
  betas <- solve.QP(Dmat/sc, -dvec/sc, Amat, b0)
  Betas <- betas$solution
  # vetores de suporte
  id.SV = Betas > 0.0001
  # plot(X[,1],X[,2], pch=19, cex=1.5, col=ifelse(Betas>0.0001,"black","lightblue"))
  
  termo1 = kernel_func(sigma,X[1, ], X[1, ])
  termo3 = t(Betas) %*% t(apply(X, 1, function(p)
    apply(X, 1, function(q)
      kernel_func(sigma,p, q)))) %*% Betas
  # funcao de raio, mede a distancia de um ponto x ao centro da estrutura 
  # obtida pela otimizacao
  R_func <- function(x) {
    termo2 = -2 * sum(Betas * apply(X, 1, function(X)
      kernel_func(sigma,X, x)))
    return(termo1 + termo2 + termo3)
  }
  Rs = apply(X, 1, R_func)
  #raio
  R = (Rs[min((1:nrow(X))[id.SV])])
  # matriz de adjacencia
  pares <- combn(nrow(X), 2) # todas as combinacoes de pares de pontos
  A = matrix(0, nrow = nrow(X), ncol = nrow(X))
  tt = seq(0, 1, length.out = varredura)
  # para cada par de pontos, são criados segmentos intermediarios e
  # testados na funcao R. Se o maior valor de R(x) no segmento for menor que R,
  # os pontos são conectados na matriz de adjacencia.
  # A matriz A representa um grafo onde Aij = 1 significa que
  # i e j pertencem ao mesmo grupo.
  # A é uma matriz binária que representa as conexões entre os pontos no grafo.
  # Se dois pontos fazem parte do mesmo grupo (conectados por
  # um caminho onde R(x) nunca ultrapassa R). Caso contrario Aij = 0.
  # Isso significa que os agrupamentos são definidos não por distância
  # diretamente, mas pela conectividade entre pontos no espaço kernel.
  for (j in 1:ncol(pares)) {
    vector1 = X[pares[, j][1], ]
    vector2 = X[pares[, j][2], ]
    eq.segmento <- function(tt)
      tt * vector1 + (1 - tt) * vector2
    segs = t(Vectorize(eq.segmento)(tt))
    R_func(unlist(segs[1, ]))
    R_max <- max(apply(segs, 1, function(a)
      R_func(unlist(a))))
    A[pares[, j][1], pares[, j][2]] = ifelse(R_max < R, 1, 0)
  }
  # criacao do grafo não direcionado baseado na matriz de adjacencia
  graph <- graph_from_adjacency_matrix(A, mode = "max", diag = FALSE)
  components <- components(graph) #identifica grupos de pontos conectados
  grupos = components$membership
  end <- Sys.time()
  exec_time <- end-start
  return(list(grupos=grupos,exec_time=exec_time))
}

varredura <- 3
sigmas <- seq(from = 2, to =3, by = 0.1)
# sigma = 3
nomes_col <- paste("cluster",sigmas,varredura)
combinacoes <- expand.grid(varredura = varredura, sigma = sigmas)
svc_ij <- matrix(0, nrow = nrow(combinacoes), ncol = 4)
grupos_ij <-  matrix(0,nrow = nrow(X),ncol=nrow(combinacoes))
# matriz com valores de varredura, sigma, exec_time e grupos
for (k in 1:nrow(combinacoes)) {
  varredura_val <- combinacoes$varredura[k]
  sigma_val <- combinacoes$sigma[k]
  svc_result <- SVC(X, sigma = sigma_val, varredura = varredura_val)
  
  svc_ij[k, 1] <- varredura_val
  svc_ij[k, 2] <- sigma_val
  svc_ij[k, 3] <- svc_result$exec_time
  svc_ij[k, 4] <- length(unique(svc_result$grupos))
  colnames(svc_ij) <- c("Varredura","Sigma","Execution Time","Clusters")
  grupos_ij[,k] <- svc_result$grupos
  print(paste("ok", k))
}
colnames(grupos_ij) <- nomes_col
svc_ij

new_df <- cbind(df,grupos_ij-1)
grafs <- list()
for (i in nomes_col){
  a <- new_df %>% ggplot(aes(x=Feature1,y=Feature2,colour= factor(new_df[,i])))+
    geom_point(alpha = 0.5, size = 4) +
    ggtitle("Agrupamento por vetores de suporte") +
    theme_minimal() +
    scale_color_manual(
      values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
    )
  grafs[i] <- list(a)
}

grafs$`cluster 2 3`
grafs[1]
# grid.arrange(for( i in grafs){i},nrow=ifelse((length(grafs)/2)%%2>0,round(length(grafs)/2,0)+1,round(length(grafs)/2)))
# for( i in grafs){
#   print(i)
# }

g1 <- new_df %>%  ggplot(aes(x=Feature1,y=Feature2,colour= factor(`cluster 2 3`)))+
  geom_point(alpha = 0.5, size = 4) +
  ggtitle("Agrupamento por vetores de suporte com sigma = 2") +
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
  )
g2 <- new_df %>%  ggplot(aes(x=Feature1,y=Feature2,colour= factor(`cluster 2.25 3`)))+
  geom_point(alpha = 0.5, size = 4) +
  ggtitle("Agrupamento por vetores de suporte com sigma = 2.25") +
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
  )
g3 <- new_df %>%  ggplot(aes(x=Feature1,y=Feature2,colour= factor(`cluster 2.5 3`)))+
  geom_point(alpha = 0.5, size = 4) +
  ggtitle("Agrupamento por vetores de suporte com sigma = 2.5") +
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
  )
g4 <- new_df %>%  ggplot(aes(x=Feature1,y=Feature2,colour= factor(`cluster 2.75 3`)))+
  geom_point(alpha = 0.5, size = 4) +
  ggtitle("Agrupamento por vetores de suporte com sigma = 2.75") +
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
  )
g5 <- new_df %>%  ggplot(aes(x=Feature1,y=Feature2,colour= factor(`cluster 3 3`)))+
  geom_point(alpha = 0.5, size = 4) +
  ggtitle("Agrupamento por vetores de suporte com sigma = 3") +
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red", setNames(rainbow(15), as.character(2:16)))
  )
grid.arrange(g1,g2,g3,g4,g5,nrow=2)
ari1 <- adjustedRandIndex(x=new_df$Classe,y=new_df$`cluster 2 3`)
ari2 <- adjustedRandIndex(x=new_df$Classe,y=new_df$`cluster 2.25 3`)
ari3 <- adjustedRandIndex(x=new_df$Classe,y=new_df$`cluster 2.5 3`)
ari4 <- adjustedRandIndex(x=new_df$Classe,y=new_df$`cluster 2.75 3`)
ari5 <- adjustedRandIndex(x=new_df$Classe,y=new_df$`cluster 3 3`)
d1 <- dunn(clusters = new_df$`cluster 2 3`,Data = X)
d2 <- dunn(clusters = new_df$`cluster 2.25 3`,Data = X)
d3 <- dunn(clusters = new_df$`cluster 2.5 3`,Data = X)
d4 <- dunn(clusters = new_df$`cluster 2.75 3`,Data = X)
d5 <- dunn(clusters = new_df$`cluster 3 3`,Data = X)
sil1 <- silhouette(new_df$`cluster 2 3`,dist(X))
sil2 <- silhouette(new_df$`cluster 2.25 3`,dist(X))
sil3 <- silhouette(new_df$`cluster 2.5 3`,dist(X))
sil4 <- silhouette(new_df$`cluster 2.75 3`,dist(X))
sil5 <- silhouette(new_df$`cluster 3 3`,dist(X))
mean(sil1[,3])
mean(sil2[,3])
mean(sil3[,3])
mean(sil4[,3])
mean(sil5[,3])
data.frame(ARI = c(ari1,ari2,ari3,ari4,ari5),
           DUNN = c(d1,d2,d3,d4,d5),
           SILHUETA = c(mean(sil1[,3]),mean(sil2[,3]),mean(sil3[,3]),mean(sil4[,3]),mean(sil5[,3])))

a1 <- fviz_silhouette(sil1)
a2 <- fviz_silhouette(sil2)
a3 <- fviz_silhouette(sil3)
a4 <- fviz_silhouette(sil4)
a5 <- fviz_silhouette(sil5)
grid.arrange(a1,a2,a3,a4,a5,nrow=2)
# inercia oe a variabilidade dada a matriz de contagem. 
# no código é a "variância"

# require(rgl)
# plot3d(X[,1],X[,2], col=grupos+1, size=10)
# plot(X,col=grupos+1,pch=19,cex=2)
# dim(table(grupos))

# df["cluster"] = factor(grupos - 1)