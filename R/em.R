#' @title A Generic EM Algorithm
#' @author Dongjie Wu
#' @description A generic EM algorithm that can work on specific models/objects.
#' @importFrom stats coef dbinom dnorm dpois kmeans logLik 
#' @importFrom stats model.frame model.matrix model.response nobs predict 
#' @importFrom stats printCoefmat pt rmultinom dmultinom
#' @export
em <- function(object, ...) {
  UseMethod("em")
}

#' The default em function
#' @param object the model used, e.g. `lm`, `glm`, `gnm`.
#' @param ... arguments used in the `model`.
#' @param latent the number of latent classes.
#' @param verbose `True` to print the process of convergence.
#' @param init.method the initialization method used in the model.
#' The default method is `random`.
#' @param max_iter the maximum iteration for em algorithm.
#' @param algo the algorithm used in em: the default EM algorithm, 
#' the classification em `cem`, or the stochastic em `sem`.
#' @param concomitant the formula to define the concomitant part of the model.
#' The default is NULL.  
#' @return the fitting object for the model with the class `em`.
#' @export
em.default <- function(object, latent=2, verbose=F,
               init.method = c("random", "kmeans"), 
               algo= c("em", "cem", "sem"),
               max_iter=500, concomitant=list(...), ...)
{
      if(!missing(...)) warning("extra arguments discarded")
      algo <- match.arg(algo)
      if (!("weights" %in% names(formals(match.fun(object$call[[1]]))))) {
        warning("The model cannot be weighted. Changed to `sem` instead.")
        algo <- "sem"
      }
      cl <- match.call()
      m <- match(c("call", "terms", "formula", "model", "y", "data", "x"), names(object), 0L)
      if (is.na(m[[1]])) {
        warning("There is no `call` for the model used.")
      } else if (is.na(m[[2]])) {
        warning("There is no `terms` for the model used.")
      } else {
        mt <- object[m]
      }
      #attr(mt$terms, ".Environment") <- environment() # attached to the current env
      if (is.null(mt$model)) {
        mf <- mt$call
        mm <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
                    names(mf), 0L)
        mf <- mf[c(1L, mm)]
        mf[[1L]] <- quote(stats::model.frame)
        mf <- eval(mf, parent.frame())
        mt$model <- mf
      }
      n <- nrow(mt$model)
      mt$x <- model.matrix(mt$terms, mt$model)
      mt$y <- model.response(mt$model)
      #mt$y <- as.double(mt$y[,2])
      

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
        models[[i]] <- mt
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
        for (i in (1:length(results))) {
          print(results[[i]]$coefficients)
        }
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
        if (algo=="cem")     {
          post_pr <- cstep(post_pr)
        }
        else if (algo=="sem") {
          post_pr <- sstep(post_pr)
        }
        ll <- 0
        if (length(concomitant)==0) {
          for (i in 1:length(results)) {
            if (pi[[i]] != 0) {
                ll <- ll + pi[[i]]*fit.den(results[[i]]) 
            }
          }
          ll <- sum(log(ll))
        } else {
          for (i in 1:length(results)) {
            if (any(!is.na(results[[i]]))) {
              ll <- ll + results.con$fitted.values[,i]*fit.den(results[[i]])
            }
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
                algorithm=algo,
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


#' @export
print.em <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  for (i in 1:length(x$models)) {
    cat(paste0("Component ", as.character(i), ":\n\n"))
    print(x$models[[i]])
  }
  cat("\n")
  if (!is.null(x$concomitant)) {
    cat("Concomitant: \n")
    print(x$results.con)
  }
  invisible(x)
}


#' @export
summary.em <- function(object, ...){
  ans = list(call=object$call,
             coefficients= list(),
             pi=object$pi,
             latent=object$latent,
             ll=0)
  names_coef <- c()
  for (i in 1:length(object$models)){
    #browser()
    if (any(!is.na(object$models[[i]]))) {
      ans$sum.models[[i]] <- summary(object$models[[i]])
      names_coef <- c(names_coef,
                    paste(as.character(i),
                          names(coef(object$models[[i]])),sep="."))
      ans$coefficients[[i]] <- coef(ans$sum.models[[i]])
    }
    #ans$ll <- ans$ll + log(ans$pi[[i]]) + logLik(object$models[[i]])
    #ans$ll <- ans$ll + sum(object$models[[i]]$weights*
    #                         (log(ans$pi[[i]])+log(fit.den(object$models[[i]]))))

    #ans$ll <- ans$ll + ans$pi[[i]]*fit.den(object$models[[i]])
  }
  if (length(object$concomitant)!=0) {
    ans$concomitant <- object$concomitant
    ans$concomitant.summary <- summary(object$results.con)
    c.df <- nrow(ans$concomitant.summary$fitted.values) -
                 ans$concomitant.summary$rank
    c.coef <- c()
    c.std <- c()
    c.tval <- c()
    c.pval <- c()
    c.names <- c()
    for (i in 1:(ans$latent-1)) {
      na <- paste(rownames(ans$concomitant.summary$coefficients)[[i]],
                  colnames(ans$concomitant.summary$coefficients), sep='.')
      c.names <- c(c.names, na)
      c.coef <- c(c.coef, ans$concomitant.summary$coefficients[i,])
      c.std <-  c(c.std, ans$concomitant.summary$standard.errors[i,])
    }
    c.tval <- c(c.tval, c.coef/c.std)
    c.pval <- c(c.pval, 2*pt(-abs(c.tval), c.df))
    coef.con <- t(rbind(c.coef, c.std, c.tval, c.pval))
    rownames(coef.con) <- c.names
    colnames(coef.con) <- c("Estimate", "Std. Error",
                            "t value", "Pr(>|t|)")
    ans$concomitant.coef <- coef.con
  }
  ans$ll <- logLik(object)
  ans$df <- attr(ans$ll, "df")
  ans$coefficients <- do.call(rbind, ans$coefficients)
  rownames(ans$coefficients) <- names_coef
  ans$obs <- object$obs
  ans$AIC <- 2 * ans$df - 2 * ans$ll
  ans$BIC <- ans$df * log(ans$obs) - 2 * ans$ll
  class(ans) <- c("summary.em")
  ans
}

#' @export
print.summary.em <-
  function (x, digits = max(3L, getOption("digits") - 3L),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"),	...)
  {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  # Coefficients:
  cat("Coefficients: \n")
  coefs <- x$coefficients
  printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
               na.print = "NA", ...)
  cat("\n")
  # Prior Probability:
  cat("Prior Probabilities: \n")
  for (i in 1:length(x$pi)) {
    cat(paste0("Comp.", i, ": ", format(x$pi[[i]], digits=digits)))
    cat("\n")
  }
  cat("\n")
  if (length(x$concomitant)!=0) {
    cat("Concomitant model: \n")
    printCoefmat(x$concomitant.coef, digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  cat("\n")
  # ll, aic, bic
  cat(paste0("logLik: ", format(x$ll, digits=digits), ", ",
             "AIC: ", format(x$AIC, digits=digits), ", ",
             "BIC: ", format(x$BIC, digits=digits), ". \n"))

  invisible(x)
  }


## Predict em:
#' @export
predict.em <- function(object, prob=c("prior", "posterior"), ...) {
  compo <- list()
  prob <- match.arg(prob)
  for (i in 1:length(object$models)){
    if (any(!is.na(object$models[[i]]))) {
      compo[[i]] <- predict(object$models[[i]])
    }
  }
  if (prob=="prior") {
    if (length(object$concomitant)==0) {
      pred = matrix(unlist(compo), ncol=object$latent) %*% object$pi
    } else {
      pred = matrix(unlist(compo), ncol=object$latent) * object$results.con$fitted.values
    }
  } else {
    pred = matrix(unlist(compo), ncol=object$latent) * object$post_pr
  }
  z <- list(components=compo,
            mean=pred,
            prob=prob)
  nprob <- paste("predict", prob, sep='.')
  class(z) <- c("predict", "predict.em", nprob)
  z
}
