em.clogit <- function(object, latent=2, verbose=F,
                       init.method = c("random", "kmeans"),
                       max_iter=500, concomitant=list(...), ...)
{
  if(!missing(...)) warning("extra arguments discarded")
  cl <- match.call()
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
  n <- nrow(mt$model) #id
  mt$x <- model.matrix(mt$terms, mt$model)
  mt$y <- model.response(mt$model)
  
  
  ## load the concomitant model
  if (length(concomitant) != 0)
  {
    m.con <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
                   names(concomitant), 0L)
    mf.con <- concomitant[m.con]
    mf.con$drop.unused.levels <- TRUE
    mf.con <- do.call(model.frame, mf.con)
    mt.con <- attr(mf.con, "terms")
  }
  #### TODO: use init.em for init_pr
  
  post_pr <- matrix(0, nrow=n, ncol=latent)
  class(post_pr) <- match.arg(init.method)
  post_pr <- init.em(post_pr, mt$x)
  #post_pr <- vdummy(sample(1:latent, size=n, replace=T))
  models <- list()
  for (i in 1:latent) {
    models[[i]] <- object
  }
  results.con <- NULL
  cnt <- 0
  conv <- 1
  llp <- 0
  while((abs(conv) > 1e-4) & (max_iter > cnt)) {
    pi_matrix <- matrix(colSums(post_pr)/nrow(post_pr),
                        nrow=nrow(post_pr), ncol=ncol(post_pr),
                        byrow=T)
    results <- mstep(models, post_pr=post_pr)
    if (length(concomitant)!=0) {
      if ("formula" %in% names(concomitant)) {
        results.con <- mstep.concomitant(concomitant$formula, mf.con, post_pr)
        pi_matrix <- results.con$fitted.values
      } else {
        stop("concomitant need to be a formula")
      }
    }
    pi <- colSums(pi_matrix)/sum(pi_matrix)
    post_pr <- estep(results, pi_matrix)
    ll <- 0
    if (length(concomitant)==0) {
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
  if (length(concomitant)!=0) {
    z$results.con=mstep.concomitant.refit(concomitant$formula, mf.con, post_pr)
    z$terms.con=mt.con
  }
  class(z) <- c("em")
  return(z)
}