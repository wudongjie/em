#' @title A Generic EM Algorithm
#'
#' @author Dongjie Wu
#'
#' @description A generic EM algorithm that can work on specific models/classes,
#' models/classes that can use `logLik` to extract the log-likelihood function.
#'
#' @return the fitting result for the model.
#'
#'
#' @export
em <- function(model, ..., latent=2, verbose=F,
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
        ll <- 0
        for (i in 1:length(results)) {
          ll <- ll + pi[[i]]*fit.den(results[[i]])
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
summary.em <- function(object){
  ans = list(call=object$call,
             coefficients= list(),
             pi=object$pi,
             latent=object$latent,
             ll=0)
  names_coef <- c()
  for (i in 1:length(object$models)){
    #browser()
    ans$sum.models[[i]] <- summary(object$models[[i]])
    names_coef <- c(names_coef,
                    paste(as.character(i),
                          names(coef(object$models[[i]])),sep="."))
    ans$coefficients[[i]] <- coef(ans$sum.models[[i]])
    #ans$ll <- ans$ll + log(ans$pi[[i]]) + logLik(object$models[[i]])
    #ans$ll <- ans$ll + sum(object$models[[i]]$weights*
    #                         (log(ans$pi[[i]])+log(fit.den(object$models[[i]]))))

    #ans$ll <- ans$ll + ans$pi[[i]]*fit.den(object$models[[i]])
  }
  if (!is.null(object$concomitant)) {
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
  #ans$ll <- sum(log(ans$ll))
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
  if (!is.null(x$concomitant)) {
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
    compo[[i]] <- predict(object$models[[i]])
  }
  if (prob=="prior") {
    pred = matrix(unlist(compo), ncol=object$latent) %*% object$pi
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
