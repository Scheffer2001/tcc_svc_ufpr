#' @title Support Vector Clustering
#'
#' @description
#' In this function, we propose another clustering method, based on the support vector machine theory,
#' the Support Vector Clustering. The algorithm applies a kernel function on the data,
#' and in the feature space we determine the support vectors, by numerical methods.
#' This SV's will help us to find data points that lie in the same cluster.
#'
#' @param data A Data frame or matrix with numerical data.
#' @param sigma A numerical value of the sigma parameter of kernel function.
#' @param varredura An integer value of the number of points that will be randomly selected in a [0,1] interval.
#' They will set the line segments between every data point, and will help to
#' build the adjacency matriz that verifies if two data points are in the same cluster or not.
#' @param kernel A character string of the type of kernel that will be applied in the data: Must be
#' one of "gaussian", "laplaciano" or "cauchy" (default is "gaussian").
#' @param n_row An integer value, if n_row is greater than 0, the data will be splitted in two
#' new dataset: in the dataset with number of rows equal to n_row, the support
#' vector clustering algorithm will be applied. In the other dataset with
#' number of rows equal to n - n_row, a support vector machine algorithm
#' will be applied, based on the clusters defined by the SVC. Use this
#' parameter when your dataset is too large to do a faster support vector clustering
#' (default is NULL).
#' @param metric A character strin of the type of metric that will be evalluated in the algorithm: must
#' be "silhouette" or "dunn" (default is "silhouette").
#' @param verbose A boolean value, if you want to see the `osqp` parameters and iterations, must be `TRUE`.
#' If you want to hide this information, `FALSE` (default is `TRUE`).
#' @returns
#' \itemize{
#'    \item `$grupos_final`: The final clusters.
#'    \item `$exec_time`: Execution time.
#'    \item `$SV`: The support vectors.
#'    \item `$SV_svm`: The support vectors founded by the SVM algorithm (n_row > 0 ).
#'    \item `$Final_data`: The final data frame before the SVM is applied (n_row > 0).
#' }
#' @importFrom osqp osqp osqpSettings
#' @importFrom clValid dunn
#' @importFrom dplyr sample_n anti_join
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom cluster silhouette
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom igraph graph_from_adjacency_matrix components
#' @importFrom e1071 svm
#' @importFrom Matrix Matrix
#' @importFrom methods as
#' @importFrom stats dist runif predict
#' @export
#'
#' @examples
#' \dontrun{
#' res <- SVC(iris[,1:4],sigma=2,varredura=10)
#' plot(iris[,1:4],col=res$grupos+1,pch=19,"Support Vector Clustering of Iris dataset")
#' iris$clusters <- res$grupos
#' res$exec_time
#' iris$SV_svc <- res$SV
#' # If n_row > 0
#' iris$SV_svm <- 0
#' iris$SV_svm[res$SV_svm] <- 1
#' library(ggplot2)
#' library(gridExtra)
#' a1 <- ggplot(iris, aes(x = iris$Sepal.Length, y = iris$Sepal.Width, color = factor(SV_svc))) +
#'  geom_point(size = 2) +
#'  labs(title = "Support Vectors from SVC", color = "SV_svc") +
#'  theme_minimal()
#' a2 <- ggplot(iris, aes(x = iris$Sepal.Length, y = iris$Sepal.Width, color = factor(SV_svm))) +
#'  geom_point(size = 2) +
#'  labs(title = "Support Vectors from SVM", color = "SV_svm") +
#'  theme_minimal()
#' grid.arrange(a1,a2)
#' }
SVC <-
function(data, sigma, varredura, kernel = "gaussian", n_row = NULL,
                metric = "silhouette", verbose = TRUE) {
  start <- Sys.time()

  calcular_metrica <- function(grupos, dados) {
    if (length(unique(grupos)) < 2) return(0)
    dist_matrix <- dist(dados)
    switch(metric,
           "silhouette" = {
             sil <- tryCatch(cluster::silhouette(grupos, dist_matrix), error = function(e) return(NULL))
             if (is.null(sil)) return(0)
             return(mean(sil[, 3]))
           },
           "dunn" = {
             val <- tryCatch(clValid::dunn(dist_matrix, grupos), error = function(e) return(0))
             return(val)
           },
           stop("Métrica inválida. Use 'silhouette' ou 'dunn'")
    )
  }

  original_data <- data
  if (!is.null(n_row) && n_row < nrow(data)) {
    X <- sample_n(data, size = n_row)
    x_out <- anti_join(data, X)
  } else {
    X <- data
    x_out <- NULL
  }

  n <- nrow(X)
  kernel_func <- switch(kernel,
                        "gaussian" = function(sigma, x, y) exp(-sigma * sum((as.numeric(x) - as.numeric(y))^2)),
                        "cauchy" = function(sigma, x, y) 1 / (1 + sigma * sum((as.numeric(x) - as.numeric(y))^2)),
                        "laplaciano" = function(sigma, x, y) exp(-sigma * sqrt(sum((as.numeric(x) - as.numeric(y))^2))),
                        stop("Kernel inválido")
  )

  Kij <- outer(1:n, 1:n, Vectorize(function(i, j) kernel_func(sigma, X[i, ], X[j, ])))
  Kjj <- diag(Kij)
  Dmat <- Kij
  dvec <- Kjj

  A.l1 <- rep(1, n)
  A.l2 <- -rep(1, n)
  A.p1 <- diag(n)
  A.p2 <- -diag(n)
  Amat <- t(rbind(A.l1, A.l2, A.p1, A.p2))
  b0 <- c(1, -1, rep(0, n), rep(-1, n))

  model <- osqp::osqp(P = Dmat, q = dvec, A = t(Amat), l = b0, u = rep(Inf, length(b0)),
                osqp::osqpSettings(verbose = FALSE))
  res <- model$Solve()
  Betas <- res$x
  id.SV <- Betas > 1e-4

  termo1 <- kernel_func(sigma, X[1, ], X[1, ])
  termo3 <- t(Betas) %*% outer(1:n, 1:n, Vectorize(function(i, j) kernel_func(sigma, X[i, ], X[j, ]))) %*% Betas
  R_func <- function(x) {
    termo2 <- -2 * sum(Betas * apply(X, 1, function(xi) kernel_func(sigma, xi, x)))
    return(termo1 + termo2 + termo3)
  }

  Rs <- apply(X, 1, R_func)
  R <- Rs[min((1:nrow(X))[id.SV])]
  A <- matrix(0, n, n)
  pares <- combn(n, 2)
  cores <- parallel::detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  tt <- runif(varredura)

  resultados <- foreach(j = 1:ncol(pares), .combine = rbind) %dopar% {
    i1 <- pares[1, j]
    i2 <- pares[2, j]
    segs <- t(sapply(tt, function(t) t * as.numeric(X[i1, ]) + (1 - t) * as.numeric(X[i2, ])))
    R_max <- max(apply(segs, 1, R_func))
    if (R_max < R) return(c(i1, i2)) else return(NULL)
  }

  stopCluster(cl)
  if (!is.null(resultados)) {
    for (i in 1:nrow(resultados)) {
      A[resultados[i, 1], resultados[i, 2]] <- 1
    }
  }
  A <- A + t(A)
  graph <- graph_from_adjacency_matrix(A, mode = "undirected")
  componentes <- components(graph)
  grupos_raw <- componentes$membership
  grupos_final <- grupos_raw
  final_data <- 0
  SV_svm <- 0
  if (!is.null(x_out)) {
    X_svm <- as.data.frame(X)
    x_out_pred <- as.data.frame(x_out)
    colnames(x_out_pred) <- colnames(X_svm)
    X_svm$cluster_label <- factor(grupos_final)
    svm_model <- tryCatch({
      e1071::svm(cluster_label ~ ., data = X_svm)
    }, error = function(e) NULL)
    SV_svm <- NULL
    if (!is.null(svm_model)) {
      x_out_pred$cluster_label <- predict(svm_model, x_out_pred)
      final_data <- rbind(X_svm, x_out_pred)
      SV_svm <- svm_model$index
    } else {
      final_data <- cbind(X, cluster_label = grupos_final)
      SV_svm <- 0
    }}

  end <- Sys.time()
  return(list(grupos = grupos_final,exec_time = end - start,
              SV = ifelse(id.SV, 1, 0),SV_svm=SV_svm,final_data = final_data))
}
