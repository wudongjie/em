#' A generic CEM algorithm
#' @description A generic CEM algorithm that can work on specific models/classes,
#' models/classes that can use `logLik` to extract the log-likelihood function.
#' @param model the model used, e.g. `lm`, `glm`, `gnm`.
#' @param ... arguments used in the `model`.
#' @param latent the number of latent classes.
#' @param verbose `True` to print the process of convergence.
#' @param init.method the initialization method used in the model.
#' The default method is `random`.
#' @param max_iter the maximum iteration for em algorithm.
#' @param concomitant the formula to define the concomitant part of the model.
#' The default is NULL.  
#' @return the fitting object for the model with the class `em` and `cem`.
#'
#'
#' @export
cem <- function(model, ..., latent=2, verbose=F,
                init.method = c("random", "kmeans"),
                max_iter=500, concomitant=NULL)
{
  cl <- match.call()
  fit <- do.call(model, list(...))
  args <- list(...)
  em.formula <- args$formula
  m <- match(c("call", "terms","model", "y", "data", "x"), names(fit), 0L)
  if (is.na(m[[1]])) {
    warning("There is no `call` for the model used.")
  } else if (is.na(m[[2]])) {
    warning("There is no `terms` for the model used.")
  } else {
    mt <- fit[m]
  }
  attr(mt$terms, ".Environment") <- environment() # attached to the current env
  mlabel <- c("model", "data")
  n <- NA
  for (l in names(mt)) {
    if (l %in% mlabel) {
      if (is.null(dim(mt[[l]]))) {
        n <- length(mt[[l]])
      } else {
        n <- nrow(mt[[l]])
      }
      break
    }
  }
  if (is.na(n)) {
    stop("Data is not loaded. Check if the model function return
             `model`, `data`, `x` or `y`.")
  }
  if (!is.null(mt$model)) {
    mt$x <- model.matrix(mt$terms, mt$model)
    mt$y <- model.response(mt$model)
  }
  else {
    mt$x <- model.matrix(mt$terms, mt$data)
    mt$y <- model.response(mt$data)
  }
  
  ## load the concomitant model
  if (!is.null(concomitant))
  {
    mf.con <- match.call()
    m.con <- match(c("concomitant", "data", "subset", "weights", "na.action", "offset"),
                   names(mf.con), 0L)
    mf.con <- mf.con[c(1L, m.con)]
    mf.con$drop.unused.levels <- TRUE
    mf.con[[1L]] <- quote(model.frame)
    names(mf.con)[names(mf.con) == "concomitant"] <- "formula"
    mf.con <- eval(mf.con, parent.frame())
    mt.con <- attr(mf.con, "terms")
  }
  #### TODO: use init.em for init_pr
  post_pr <- matrix(0, nrow=n, ncol=latent)
  class(post_pr) <- match.arg(init.method)
  post_pr <- init.em(post_pr, mt$x)
  #post_pr <- vdummy(sample(1:latent, size=n, replace=T))
  models <- list()
  for (i in 1:latent) {
    models[[i]] <- model
  }
  results.con <- NULL
  cnt <- 0
  conv <- 1
  llp <- 0
  while((abs(conv) > 1e-4) & (max_iter > cnt)) {
    pi_matrix <- matrix(colSums(post_pr)/nrow(post_pr),
                        nrow=nrow(post_pr), ncol=ncol(post_pr),
                        byrow=T)
    results <- mstep(models, args, post_pr=post_pr)
    if (!is.null(concomitant)) {
      if ("formula" %in% class(concomitant)) {
        results.con <- mstep.concomitant(concomitant, mf.con, post_pr)
        pi_matrix <- results.con$fitted.values
      } else {
        stop("concomitant need to be a formula")
      }
    }
    pi <- colSums(pi_matrix)/sum(pi_matrix)
    post_pr <- estep(results, pi_matrix)
    post_pr <- cstep(post_pr)
    ll <- 0
    if (is.null(concomitant)) {
      for (i in 1:length(results)) {
        ll <- ll + pi[[i]]*fit.den(results[[i]])
      }
      ll <- sum(log(ll))
    } else {
      for (i in 1:length(results)) {
        ll <- ll + results.con$fitted.values[,i]*fit.den(results[[i]])
      }
      ll <- sum(log(ll))
    }
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
            init.method = match.arg(init.method),
            call=cl,
            terms=mt$terms,
            obs=n,
            post_pr=estep(results, pi_matrix),
            concomitant=concomitant)
  if (!is.null(concomitant)) {
    z$results.con=mstep.concomitant.refit(concomitant, mf.con, post_pr)
    z$terms.con=mt.con
  }
  class(z) <- c("em", "cem")
  return(z)
}
