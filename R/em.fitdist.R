#' The default em function
#' @param object the model used, e.g. `lm`, `glm`, `gnm`.
#' @param ... arguments used in the `model`.
#' @param latent the number of latent classes.
#' @param verbose `True` to print the process of convergence.
#' @param init.method the initialization method used in the model.
#' The default method is `random`. `kmeans` is K-means clustering.
#' `hc` is model-based agglomerative hierarchical clustering.
#' @param init.prob the starting prior probabilities used in classification based method.
#' @param max_iter the maximum iteration for em algorithm.
#' @param algo the algorithm used in em: `em` the default EM algorithm,
#' the classification em `cem`, or the stochastic em `sem`.
#' @return the fitting object for the model with the class `em`.
#' @export
em.fitdist <- function(object, latent = 2, verbose = FALSE,
                       init.method = c("random", "kmeans", "hc"), init.prob = NULL,
                       algo = c("em", "cem", "sem"),
                       max_iter = 500, ...) {
  args <- list()
  # browser()
  args$data <- object$data
  args$method <- object$method
  args$distname <- object$distname
  args$fix.arg <- object$fix.arg
  args$discrete <- object$discrete
  object$start <- list(unname(object$estimate))
  names(object$start) <- names(object$estimate)
  cl <- quote(fitdist(data, distr = distname, method = method, fix.arg = fix.arg, discrete = discrete, start = start))
  cl <- as.call(cl)
  mt <- list()
  mt$model <- object$data
  mt$call <- cl
  n <- object$n
  # browser()
  post_pr <- matrix(0, nrow = n, ncol = latent)
  class(post_pr) <- match.arg(init.method)
  post_pr <- init.em(post_pr, mt$x)
  # post_pr <- vdummy(sample(1:latent, size=n, replace=T))
  models <- list()
  for (i in 1:latent) {
    models[[i]] <- mt
  }
  results.con <- NULL
  cnt <- 0
  conv <- 1
  llp <- 0
  while ((abs(conv) > 1e-4) & (max_iter > cnt)) {
    pi_matrix <- matrix(colSums(post_pr) / nrow(post_pr),
      nrow = nrow(post_pr), ncol = ncol(post_pr),
      byrow = T
    )
    results <- mstep(models, post_pr = post_pr)
    for (i in (seq_len(length(results)))) {
      print(results[[i]]$coefficients)
    }
    pi <- colSums(pi_matrix) / sum(pi_matrix)
    post_pr <- estep(results, pi_matrix)
    if (algo == "cem") {
      post_pr <- cstep(post_pr)
    } else if (algo == "sem") {
      post_pr <- sstep(post_pr)
    }
    ll <- 0
    for (i in seq_len(length(results))) {
      if (pi[[i]] != 0) {
        ll <- ll + pi[[i]] * fit.den(results[[i]])
      }
    }
    ll <- sum(log(ll))
    conv <- ll - llp
    llp <- ll
    if (verbose) {
      cat(paste0(
        "Iteration ", cnt, ": ",
        "(EM) log likelihood = ",
        round(ll, 4), "\n"
      ))
    }
    cnt <- cnt + 1
  }
  z <- list(
    models = results,
    pi = colSums(pi_matrix) / sum(pi_matrix),
    latent = latent,
    init.method = match.arg(init.method),
    call = cl,
    terms = mt$terms,
    algorithm = algo,
    obs = n,
    post_pr = estep(results, pi_matrix)
  )
  class(z) <- c("em")
  return(z)
}
