otimizar_sigma <-
function(data, varredura, p_iniciais = 5, p_finais = 10,
                           kernel = "gaussian", metric = "silhouette", 
                           verbose = TRUE,s_lower=0.25,s_upper=10) {
  SVC_wrapper <- function(data, sigma, varredura, kernel, metric) {
    result <- tryCatch({
      SVC(data = data, sigma = sigma, varredura = varredura, 
          kernel = kernel,metric = metric, verbose = FALSE)
    }, error = function(e) list(grupos = rep(1, nrow(data))))
    metrica <- tryCatch({
      dist_matrix <- dist(data)
      sil <- cluster::silhouette(result$grupos, dist_matrix)
      mean(sil[, 3])
    }, error = function(e) 0)
    return(metrica)
  }
  
  param_set <- ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam("sigma", lower = s_lower, upper = s_upper)
  )
  
  objective_function <- smoof::makeSingleObjectiveFunction(
    name = "SVC_optim",
    fn = function(x) {
      SVC_wrapper(data = data, sigma = x["sigma"],varredura = varredura, 
                  kernel = kernel, metric = metric)
    },
    par.set = param_set,
    minimize = FALSE
  )
  
  ctrl <- mlrMBO::makeMBOControl() %>%
    mlrMBO::setMBOControlTermination(iters = p_finais) %>%
    mlrMBO::setMBOControlInfill(
      crit = mlrMBO::makeMBOInfillCritEI(),opt = "focussearch",
      opt.focussearch.points = 1000
    )
  
  design <- generateDesign(n = p_iniciais, par.set = param_set, fun = lhs::randomLHS)
  design <- data.frame(sigma = design)
  
  resultado_mbo <- mlrMBO::mbo(fun = objective_function,design = design,
                               control = ctrl,show.info = FALSE)
  if (verbose) {
    cat("\n=== OTIMIZAÇÃO FINALIZADA ===\n")
    cat("Melhor sigma:", resultado_mbo$x$sigma, "\n")
    cat("Métrica (", metric, "):", resultado_mbo$y, "\n\n")
  }
  return(resultado_mbo$x$sigma)
}
