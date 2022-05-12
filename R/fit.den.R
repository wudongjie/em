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
    if (is.null(object$weights)) {
      object$weights = 1
    }
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(object$df.residual+2))
    den <- dnorm(y, mean=predict(object), sd=sigma)
    return(den)
}

#' @export
fit.den.glm <- function(object, ...){
  if (is.null(object$weights)) {
    object$weights = 1
  }
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(object$df.residual+2))
    den <- dnorm(y, mean=predict(object), sd=sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(model.frame(object$formula, data=object$model))
    den <- dpois(y, lambda=predict(object, type="response", newdata=object$data))
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size=rep(1, ob), prob=predict(object, type="response", newdata=object$data))
  }
  return(den)
}

fit.den.gnm <- function(object, ...){
  if (is.null(object$weights)) {
    object$weights = 1
  }
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2/mean(object$weights))/(object$df.residual+2))
    den <- dnorm(y, mean=predict(object), sd=sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(object$model)
    den <- dpois(y, lambda=predict(object, type="response", newdata=object$model))
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size=rep(1, ob), prob=predict(object, type="response", newdata=object$model))
  }
  return(den)
}


#' @export
fit.den.nnet <- function(object, ...){

}

#' Fit the density for the survival::clogit
#' @export
fit.den.coxph <- function(object, ...){
  #browser()
  cl <- object$call
  cl$subset <- NULL
  cl[[1L]] <- quote(stats::model.frame)
  cl$method <- NULL
  mf <- eval(cl,  attr(object$terms, ".Environment"))
  y <- model.response(mf)
  y <- as.double(y[,2])
  x <- model.matrix.coxph(object,data=mf)
  co <- coef(object)
  co[is.na(co)] <- 0
  pred <- exp(x %*% co) / (1 + exp(x %*% co))
  gen.mn <- function(dt) {
    dmultinom(dt$y, prob=dt$fitted)
  }
  # gen.mn <- function(v1,v2) {
  #   dmultinom(v1,prob=v2)
  # }
  temp <- untangle.specials(object$terms, 'strata', 1)
  strat <- as.integer(strata(mf[temp$vars], shortlabel=T))
  # TODO: extract the column of strata

  #browser()
  #library(data.table)
  df <- data.frame(y=y, fitted=pred,strat=strat)
  #dt <- data.table(df, key="strat")
  #browser()
  #dt <- setDT(dt)
  #dt <- dt[,den:=multinom(y, prob=fitted), by=strat]
  #den <- unlist(by(dt,
  #                 strat, gen.mn, simplify = F))
  #den <- plyr::ddply(dt, ~strat, gen.mn)$V1
  (df %>% dplyr::group_by(strat) %>% dplyr::summarise(den=dmultinom(y, prob=fitted)))$den
}

#' @export
fit.den.glmerMod <- function(object, ...){
  browser()
  if (object@resp$family[1]$family == "gaussian") {
    y <- model.response(object@frame)
    sigma <- sqrt(sum(object@resp$weights * object$residuals^2/mean(object@resp$weights))/(object$df.residual-1))
    den <- dnorm(y, mean=predict(object), sd=sigma)
  } else if (object@resp$family[1]$family == "poisson") {
    y <- model.response(model.frame(object$formula, data=object@frame))
    den <- dpois(y, lambda=predict(object, type="response", newdata=object@frame, allow.new.levels = T))
  } else if (object@resp$family[1]$family == "binomial") {
    y <- model.response(object@frame)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size=rep(1, ob), prob=predict(object, type="response", newdata=object@frame, allow.new.levels = T))
  }
  return(den)
}

#' @export
fit.den.plm <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights = 1
  }
  md <- object$args$model
  ef <- object$args$effect
  y <- pmodel.response(object$model, model=md, effect=ef, theta=object$ercomp$theta)
  sigma <- sqrt(var(object$residuals)) #problem
  den <- dnorm(y, mean=predict(object, newdata=object$frame, model=md, effect=ef, theta=object$ercomp$theta), sd=sigma)
  return(den)
}
