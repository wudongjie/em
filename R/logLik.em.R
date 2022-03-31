#' @title logLik of EM Algorithm
#'
#' @author Dongjie Wu
#'
#' @description This function computes logLik of EM Algorithm.
#'
#' @return the loglikelihood value

#' @export
logLik.em <- function(object, ...) {
  p <- 0
  ll <- 0
  if (is.null(object$concomitant)) {
    for (i in 1:length(object$models)) {
      p <- p + df.em(object$models[[i]])
      #ll <- ll + object$post_pr[,i]*log(object$pi[[i]]*fit.den(object$models[[i]]))
      ll <- ll + object$pi[[i]]*fit.den(object$models[[i]])
    }
    ll <- sum(log(ll))
  } else {
    for (i in 1:length(object$models)) {
      p <- p + df.em(object$models[[i]])
      ll <- ll + object$results.con$fitted.values[,i]*fit.den(object$models[[i]])
    }
    p <- p + object$results.con$edf - 1
    ll <- sum(log(ll))
  }
  p <- p + object$latent - 1
  attr(ll, "nobs") <- object$obs
  attr(ll, "df") <- p
  class(ll) <- "logLik"
  ll
}


df.em <- function(object, ...) {
  UseMethod("df.em")
}

df.em.default <- function(object, ...) {
  npar <- length(object$coefficients)
  npar
}

df.em.lm <- function(object, ...) {
  npar <- object$rank + 1
  npar
}

df.em.glm <- function(object, ...) {
  npar <- object$rank + ifelse(object$family[1]$family == "gaussian", 1, 0)
  npar
}
