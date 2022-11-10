#' @title A Generic EM Algorithm
#' @author Dongjie Wu
#' @description This is a generic EM algorithm that can work on specific models/objects. 
#' Currently, it supports `lm`, `glm`, `gnm` in package gnm, 
#' `clogit` in package survival and `multinom` in package nnet.
#' Use `?em.default` to check the manual of the default function of `em`.
#' @importFrom stats coef dbinom dnorm dpois kmeans logLik 
#' @importFrom stats model.frame model.matrix model.response nobs predict 
#' @importFrom stats printCoefmat pt rmultinom dmultinom
#' @param object the model used, e.g. `lm`, `glm`, `gnm`, `clogit`, `multinom`
#' @param ... arguments used in the `model`.
#' @export
em <- function(object, ...) {
  UseMethod("em")
}

#' The default em function
#' @useDynLib em, .registration=TRUE
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
#' @param cluster.by a variable to define the level of clustering.
#' @param abs_tol absolute accuracy requested.
#' @param use.optim maximize the complete log likelihood (MLE) by using `optim` and `rcpp` code.The default value is `FALSE`.
#' @param optim.start the initialization method of generating the starting value for MLE.    
#' @return the fitting object for the model with the class `em`.
#' @export
em.default <- function(object, latent=2, verbose=F,
               init.method = c("random", "kmeans", "hc"), init.prob = NULL,
               algo= c("em", "cem", "sem"),  cluster.by=NULL,
               max_iter=500, abs_tol=1e-4, concomitant=list(...), 
               use.optim=F, optim.start=c("random","sample5"),
               ...)
{
      if(!missing(...)) warning("extra arguments discarded")
      algo <- match.arg(algo)
      optim.start <- match.arg(optim.start)
      callName <- deparse(object$call[[1]])
      get_args <- list(x=callName, mode="function")
      if (grepl("::", callName)) {
         funs <- strsplit(callName,split='::', fixed=TRUE)
         envName <- funs[[1]][1]
         callName <- funs[[1]][2]
         get_args$envir <- environment(quote(envName))
         get_args$x <- callName
      }
      if (!("weights" %in% names(formals(do.call(get, get_args)))) & (algo=="em")) {
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
      mt$coefficients <- object$coefficients
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
      
      if (is.null(cluster.by)) {
        np <- n
        cfreq <- 1
      } else {
        # Check cluster.by
        if (is.null(dim(cluster.by))) {
          if (length(cluster.by) != n) {
            stop("cluster.by does not match data used.")
          }
          mt$model <- mt$model[order(cluster.by),]
          cluster.by <- cluster.by[order(cluster.by)]
          np <- length(unique(cluster.by))
          cfreq <- as.data.frame(table(cluster.by))$Freq
          cfreq <- cfreq[cfreq>0]
        } else {
          if (nrow(cluster.by) != n) {
            stop("cluster.by does not match data used.")
          }
          mt$model <- mt$model[do.call(order, as.data.frame(cluster.by)),]
          cluster.by <- cluster.by[do.call(order, as.data.frame(cluster.by)),]
          np <- nrow(unique(cluster.by))
          cfreq <- as.data.frame(table(data.frame(cluster.by)))$Freq
          cfreq <- cfreq[cfreq>0]  
        }
      }
      post_pr <- matrix(0, nrow=n, ncol=latent)
      # check the degree of freedom
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
      #browser()
      lt = 1
      ln = 1
      lns = names(object$xlevels)
      chk_ft = 10
      # check factors with one level.
      bool_ft <- c()
      cond1 <- T
      #browser()
      while (cond1) {
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
        resd <- object$residuals
        post_pr <- init.em(post_pr, data=mt$x, init.prob=init.prob)
        if (identical(lns, character(0))) {
          break
        }
        for (i in 1:latent) {
          bool_ft = c(bool_ft, 
                      any(unlist(lapply(lns, function(x){
                        length(unique(subset(mt$model, 
                                             post_pr[,i]==1)[[x]]))==1
                      }))))
        }
        chk_ft = chk_ft - 1
        if (chk_ft <= 0) {
          stop("There is at least one factor with only one level!")
        }
        cond1 <- any(bool_ft)
      }
      # check whether factors have only one level.
      models <- list()
      for (i in 1:latent) {
        models[[i]] <- object
      }
      results.con <- NULL
      if (use.optim) {
          #results <- mstep(models, post_pr=post_pr)
          #pi_matrix <- matrix(colSums(post_pr)/nrow(post_pr),
          #                    nrow=nrow(post_pr), ncol=ncol(post_pr),
          #                    byrow=T)
          #post_pr <- estep(results, pi_matrix)
          if (optim.start == "sample5") {
            sample5 = T;
          } else {
            sample5= F;
          }
          results <- emOptim(models, post_pr, sample5=sample5, cluster.by=cluster.by, abs_tol=abs_tol,
                             concomitant=concomitant, mf.con=mf.con, max_iter=max_iter)
          pi_matrix <- results[[1]]$pi_matrix
          z <- list(models=results,
                    pi=colSums(pi_matrix)/sum(pi_matrix),
                    latent=latent,
                    algorithm=algo,
                    obs=n,
                    post_pr=estep(results, pi_matrix),
                    concomitant=concomitant)
      } else {
          z <- emstep(models, post_pr, n, algo=algo, cfreq=cfreq, 
                      max_iter=max_iter, abs_tol=abs_tol, concomitant=concomitant, 
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
