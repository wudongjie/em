#' Fit the density function for a fitted model.
#' @export
fit.den <- function(object, ...) {
  UseMethod("fit.den")
}

#' Fitting the density function using in `fitdistrplus::fitdist()`
#' @export
fit.den.fitdist <- function(object, ...) {
    ddistname <- paste("d", object$distname, sep="")
    argddistname <- names(formals(ddistname))
    if (!exists(ddistname, mode="function"))
      stop(paste("The ", ddistname, " function must be defined"))
    den <- do.call(ddistname,
                   c(list(object$data),
                     as.list(object$estimate),
                     as.list(object$fix.arg)))
    den
}


#' @export
fit.den.lm <- function(object, ...) {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(nobs(object) - object$rank-1))
    den <- dnorm(y, mean=object$fitted.values, sd=sigma)
    return(den)
}

#' @export
fit.den.glm <- function(object, ...){
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(nobs(object) - object$rank-1))
    den <- dnorm(y, mean=object$fitted.values, sd=sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(model.frame(object$formula, data=object$data))
    den <- dpois(y, lambda=object$fitted.values)
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size=rep(1, ob), prob=object$fitted.values)
  }
  return(den)
}

fit.den.gnm <- function(object, ...){
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(nobs(object) - object$rank-1))
    den <- dnorm(y, mean=object$fitted.values, sd=sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(object$model)
    den <- dpois(y, lambda=object$fitted.values)
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size=rep(1, ob), prob=object$fitted.values)
  }
  return(den)
}


#' @export
fit.den.nnet <- function(object, ...){

}

#' @export
fit.den.clogit <- function(object, ...){
  y <- model.response(object$model)
  if (is.null(dim(y))) {
    ob <- length(y)
  } else {
    ob <- nrow(y)
  }
  den <- dbinom(y, size=rep(1, ob), prob=object$fitted.values)
}
