#' @title A Generic CEM Algorithm
#'
#' @author Dongjie Wu
#'
#' @description A generic CEM algorithm that can work on specific models/classes,
#' models/classes that can use `logLik` to extract the log-likelihood function.
#'
#' @return the fitting result for the model.
#'
#'
#' @export
cem <- function(model, pars, latent=2, verbose=F, max_iter=500)
{
  # start
  cl <- match.call()
  tryCatch(
    {
      tmp <- do.call(model, pars)
      n <- nobs(tmp)
    },
    error=function(){
      message("The model cannot be fitted using the arguments given!")
      return(NA)
    }
  )
  init_pr <- vdummy(sample(1:latent, size=n, replace=T))
  models <- list()
  for (i in 1:latent) {
    models[[i]] <- model
  }
  results <- mstep(models, pars, post_pr=init_pr)
  pi_matrix <- matrix(rep(1.0/latent, latent), ncol=latent)
  cnt <- 0
  conv <- 1
  llp <- 0
  while((abs(conv) > 1e-4) & (max_iter > cnt)) {
    post_pr <- estep(results, pi_matrix)
    post_pr <- cstep(post_pr)
    pi_matrix <- matrix(colSums(post_pr)/nrow(post_pr),
                        nrow=nrow(post_pr), ncol=ncol(post_pr),
                        byrow=T)
    results <- mstep(models, pars, post_pr=post_pr)
    pi <- colSums(pi_matrix)/sum(pi_matrix)
    ll <- 0
    for (i in 1:length(results)) {
      ll <- ll + pi*fit.den(results[[i]])
    }
    ll <- sum(log(ll))
    conv <- ll - llp
    llp <- ll
    if (verbose) {
      cat(paste0("Iteration ", cnt, ": ",
                 "(EM) log likelihood = ",
                 round(ll, 4), "\n"))
    }
    cnt <- cnt + 1
  }
  z <- list(models=results,
            pi=colSums(pi_matrix)/sum(pi_matrix),
            latent=latent,
            call=cl,
            obs=nobs(results[[1]]),
            post_pr=estep(results, pi_matrix))
  class(z) <- c("cem", "em")
  return(z)
}
