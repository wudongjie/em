#' This function computes logLik of EM Algorithm.
#' @method logLik em
#' @param object an object of `em`.
#' @param ... other used arguments.
#' @return the log-likelihood value
#' @export
logLik.em <- function(object, ...) {
  p <- 0
  ll <- 0
  upost_pr <- unique(object$post_pr)
  if (length(object$concomitant) == 0) {
    for (i in seq_len(length(object$models))) {
      if (any(!is.na(object$models[[i]]))) {
        p <- p + df.em(object$models[[i]])
        # ll <- ll + object$post_pr[,i]*log(object$pi[[i]]*fit.den(object$models[[i]]))
        ll <- ll + object$pi[[i]] * fit.den(object$models[[i]])
      }
    }
    ll <- sum(log(ll))
  } else {
    for (i in seq_len(length(object$models))) {
      if (any(!is.na(object$models[[i]]))) {
        p <- p + df.em(object$models[[i]])
        ll <- ll + object$results.con$fitted.values[, i] * fit.den(object$models[[i]])
      }
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

logLik.plm <- function(object, ...) {
  out <- -plm::nobs(object) * log(2 * var(object$residuals) * pi) / 2 - deviance(object) / (2 * var(object$residuals))

  attr(out, "df") <- nobs(object) - object$df.residual
  attr(out, "nobs") <- plm::nobs(summary(object))
  return(out)
}
