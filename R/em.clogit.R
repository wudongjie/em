#' The em function for `survival::clogit`.
#' @importFrom utils methods str
#' @importFrom survival untangle.specials strata
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
#' @param concomitant the formula to define the concomitant part of the model.
#' The default is NULL.  
#' @return the fitting object for the model with the class `em`.
#' @export
em.clogit <- function(object, latent=2, verbose=F,
                       init.method = c("random", "kmeans", "hc"), init.prob = NULL,
                      algo= c("em", "cem", "sem"),
                      cluster.by=NULL, max_iter=500, concomitant=list(...), 
                      use.optim=F,  optim.start=c("random","sample5"),
                      ...)
{
  if(!missing(...)) warning("extra arguments discarded")
  cl <- match.call()
  # if (object$call$method == "exact") {
  #   warning("Method cannot be exact. Change to approximate.")
  #   object$call$method = "breslow"
  # }
  #browser()
  algo <- match.arg(algo)
  optim.start <- match.arg(optim.start)
  if (algo=="em" && use.optim==F) {
    warning("The model cannot be weighted. Changed to `sem` instead.")
    algo <- "sem"
  }
  m <- match(c("call", "terms", "formula", "model", "y", "data", "x"), names(object), 0L)
  if (is.na(m[[1]])) {
    warning("There is no `call` for the model used.")
  } else if (is.na(m[[2]])) {
    warning("There is no `terms` for the model used.")
  } else {
    mt <- object[m]
  }
  attr(mt$terms, ".Environment") <- environment() # attached to the current env
  if (is.null(mt$model)) {
    mf <- mt$call
    mm <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
                names(mf), 0L)
    mf <- mf[c(1L, mm)]
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt$model <- mf
  }
  nr <- object$n #nrow
  n <- object$nevent #strata
  #### TODO: use init.em for init_pr
  temp <- untangle.specials(object$terms, 'strata', 1)
  strat <- as.integer(strata(mf[temp$vars], shortlabel=T))
  mt$model <- mt$model[order(strat),]
  strat <- strat[order(strat)]
  strat.freq <- as.data.frame(table(strat))$Freq
  ## load the concomitant model
  if (length(concomitant) != 0)
  {
    m.con <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
                   names(concomitant), 0L)
    mf.con <- concomitant[m.con]
    if (!is.null(cluster.by)) {
      mf.con$data <- mf.con$data[!duplcated(cluster.by), ] 
    } else {
      mf.con$data <- mf.con$data[!duplicated(strat),]
    }
    mf.con$drop.unused.levels <- TRUE
    mf.con <- do.call(model.frame, mf.con)
    mt.con <- attr(mf.con, "terms")
  }
  if (is.null(cluster.by)) {
    np <- n
    cfreq <- 1
    # Look for the strata variable
  } else {
    # Check cluster.by
    if (is.null(dim(cluster.by))) {
      if (length(cluster.by) != nr) {
        stop("cluster.by does not match data used.")
      }
      mt$model <- mt$model[order(cluster.by),]
      cluster.by <- cluster.by[order(cluster.by)]
      np <- length(unique(cluster.by))
      cfreq <- as.data.frame(table(cluster.by[!duplicated(strat)]))$Freq
      cfreq <- cfreq[cfreq>0]
    } else {
      if (nrow(cluster.by) != nr) {
        stop("cluster.by does not match data used.")
      }
      mt$model <- mt$model[do.call(order, as.data.frame(cluster.by)),]
      cluster.by <- cluster.by[do.call(order, as.data.frame(cluster.by)),]
      np <- nrow(unique(cluster.by))
      cfreq <- as.data.frame(table(data.frame(cluster.by[!duplicated(strat),])))$Freq
      cfreq <- cfreq[cfreq>0]  
    }
  }
  post_pr <- matrix(0, nrow=np, ncol=latent)
  class(post_pr) <- match.arg(init.method)
  if (!is.null(init.prob)) {
    if (!is.vector(init.prob)) {
      warnings("init.prob should be a vector! Drop init.prob.")
      init.prob = NULL
    } 
    if (length(init.prob)!=latent) {
      warnings("init.prob should be equal to the number of latent classes! Dropb init.prob.")
      init.prob = NULL
    }
  }
  mt$x <- model.matrix.coxph(object, data=mt$model)
  mt$y <- model.response(mt$model)
  mt$y <- as.matrix(as.double(mt$y[,2]))
  if (!is.null(cluster.by)) {
      cid <- cluster.by
  } else {
      cid <- strat
  }
  browser()
  dat_tmp <-  as.data.frame(cbind(mt$x, mt$y,cid))
  dat_tmp <- dat_tmp %>% dplyr::group_by(cid) %>% dplyr::summarise(dplyr::across(dplyr::everything(), mean))
  #dat_tmp <- reshape(as.data.frame(dat_tmp), timevar="alt", idvar="strat", direction="wide")
  dat_tmp <- subset(dat_tmp, select=-c(cid))
  post_pr <- init.em(post_pr, data=dat_tmp, init.prob=init.prob)
  # chk_df <- 10
  # while (any(colSums(post_pr) <= length(object$coefficients))) {
  #   warnings("Lack of degree of freedom. Reinitializing...")
  #   class(post_pr) <- match.arg(init.method)
  #   post_pr <- init.em(post_pr, mt$x)
  #   chk_df <- chk_df - 1
  #   if (chk_df <= 0) {
  #     stop("Lack of degree of freedom.")
  #   }
  # }
  models <- list()
  for (i in 1:latent) {
    models[[i]] <- object
  }
  results.con <- NULL
  if (use.optim) {
    if (optim.start == "sample5") {
      sample5 = T;
    } else {
      sample5= F;
    }
    results <- emOptim(models, post_pr, algo=algo, sample5=sample5, 
                       cluster.by=cluster.by, cfreq=cfreq, 
                       concomitant=concomitant, mf.con=mf.con)
    pi_matrix <- results[[1]]$pi_matrix
    z <- list(models=results,
              pi=colSums(pi_matrix)/sum(pi_matrix),
              latent=latent,
              algorithm=algo,
              obs=n,
              post_pr=estep(results, pi_matrix),
              concomitant=concomitant)
  }
  else {
    z <- emstep(models, post_pr, n, algo=algo, cfreq=cfreq, 
                max_iter=max_iter, abs_tol=1e-4, concomitant=concomitant, 
                mf.con=mf.con, verbose=verbose)
    z$init.method = match.arg(init.method)
    z$call=cl
    z$terms=mt$terms
  }
  if (length(concomitant)!=0) {
    z$results.con=mstep.concomitant.refit(concomitant$formula, mf.con, post_pr)
    z$terms.con=mt.con
  }
  class(z) <- c("em")
  return(z)
}