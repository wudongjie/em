#' Fit the density function for a fitted model.
#' @description This function generates the probability density of given models.
#' @param object the fitted model such as `lm`.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den <- function(object, ...) {
  UseMethod("fit.den")
}

#' Fitting the density function using in `fitdistrplus::fitdist()`
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.fitdist <- function(object, ...) {
  ddistname <- paste("d", object$distname, sep = "")
  argddistname <- names(formals(ddistname))
  if (!exists(ddistname, mode = "function")) {
    stop(paste("The ", ddistname, " function must be defined"))
  }
  den <- do.call(
    ddistname,
    c(
      list(object$data),
      as.list(object$estimate),
      as.list(object$fix.arg)
    )
  )
  den
}

#' Fit the density function for a linear regression model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.lm <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(object$data[[names(object$xlevels[i])]])) !=
        length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(object$data[[names(object$xlevels[i])]])
      }
    }
  }
  y <- model.response(object$model)
  sigma <- sqrt(sum(object$weights * object$residuals^2 / mean(object$weights)) / (object$df.residual + 2))
  den <- dnorm(y, mean = predict(object), sd = sigma)
  return(den)
}

#' Fit the density function for a generalized linear regression model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.glm <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(object$data[[names(object$xlevels[i])]])) !=
        length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(object$data[[names(object$xlevels[i])]])
      }
    }
  }
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2 / mean(object$weights)) / (object$df.residual + 2))
    den <- dnorm(y, mean = predict(object), sd = sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(object$model)
    den <- dpois(y, lambda = predict(object, type = "response", newdata = object$data))
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size = rep(1, ob), prob = predict(object, type = "response", newdata = object$data))
  }
  return(den)
}

#' Fit the density function for a generalized non-linear regression model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.gnm <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(object$model[[names(object$xlevels[i])]])) !=
        length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(object$model[[names(object$xlevels[i])]])
      }
    }
  }
  if (object$family[1]$family == "gaussian") {
    y <- model.response(object$model)
    sigma <- sqrt(sum(object$weights * object$residuals^2 / mean(object$weights)) / (object$df.residual + 2))
    den <- dnorm(y, mean = predict(object), sd = sigma)
  } else if (object$family[1]$family == "poisson") {
    y <- model.response(object$model)
    den <- dpois(y, lambda = predict(object, type = "response", newdata = object$model))
  } else if (object$family[1]$family == "binomial") {
    y <- model.response(object$model)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size = rep(1, ob), prob = predict(object, type = "response", newdata = object$model))
  }
  return(den)
}

#' Fit the density function for a `nnet` model.
#' @param object the fitted model.
#' @param ... other used arguments.
fit.den.nnet <- function(object, ...) {

}

#' Fit the density function for a multinomial regression model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.multinom <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(object$data[[names(object$xlevels[i])]])) !=
        length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(object$data[[names(object$xlevels[i])]])
      }
    }
  }
  mf <- model.frame(object$terms)
  y <- model.response(mf)
  fitted <- predict(object, type = "probs", newdata = mf)
  if (is.null(dim(y))) {
    y <- vdummy(y)
  }
  den <- vapply(seq_len(nrow(fitted)), function(i) {
    dmultinom(y[i, ], prob = fitted[i, ], size = 1)
  },
  FUN.VALUE = numeric(1)
  )
  den
}

#' Fit the density for the survival::clogit
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.coxph <- function(object, ...) {
  cl <- object$call
  cl$subset <- NULL
  cl[[1L]] <- quote(stats::model.frame)
  cl$method <- NULL
  mf <- eval(cl, attr(object$terms, ".Environment"))
  y <- model.response(mf)
  y <- as.double(y[, 2])
  x <- model.matrix.coxph(object, data = mf)
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(mf[[names(object$xlevels[i])]])) !=
        length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(mf[[names(object$xlevels[i])]])
      }
    }
  }
  co <- coef(object)
  co[is.na(co)] <- 0
  # pred <- exp(x %*% co) #/ (1 + exp(x %*% co))
  l1 <- y * (x %*% co)
  l2 <- exp(x %*% co)
  # gen.mn <- function(v1,v2) {
  #   dmultinom(v1,prob=v2)
  # }
  temp <- untangle.specials(object$terms, "strata", 1)
  strat <- as.integer(strata(mf[temp$vars], shortlabel = T))
  # TODO: extract the column of strata

  #
  # library(data.table)
  # df <- data.frame(y=y, fitted=pred,strat=strat)
  df <- data.frame(l1 = l1, l2 = l2, strat = strat)
  # dt <- data.table(df, key="strat")
  # browser()
  # dt <- setDT(dt)
  # dt <- dt[,den:=multinom(y, prob=fitted), by=strat]
  # den <- unlist(by(dt,
  #                 strat, gen.mn, simplify = F))
  # den <- plyr::ddply(dt, ~strat, gen.mn)$V1
  # den <- (df %>% dplyr::group_by(strat) %>% dplyr::summarise(den=dmultinom(y, prob=fitted)))$den
  group_sum <- function(l1, l2) {
    sum(l1) - log(sum(l2))
  }
  den <- (df %>% dplyr::group_by(strat) %>% dplyr::summarise(den = group_sum(l1, l2)))$den
  exp(den)
}

#' Fit the density function for a generalized linear mixed effect model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.glmerMod <- function(object, ...) {
  # browser()
  if (object@resp$family[1]$family == "gaussian") {
    y <- model.response(object@frame)
    sigma <- sqrt(sum(object@resp$weights * object$residuals^2 / mean(object@resp$weights)) / (object$df.residual - 1))
    den <- dnorm(y, mean = predict(object), sd = sigma)
  } else if (object@resp$family[1]$family == "poisson") {
    y <- model.response(object@frame)
    den <- dpois(y, lambda = predict(object, type = "response", newdata = object@frame, allow.new.levels = T))
  } else if (object@resp$family[1]$family == "binomial") {
    y <- model.response(object@frame)
    if (is.null(dim(y))) {
      ob <- length(y)
    } else {
      ob <- nrow(y)
    }
    den <- dbinom(y, size = rep(1, ob), prob = predict(object, type = "response", newdata = object@frame, allow.new.levels = T))
  }
  return(den)
}

#' Fit the density function for a panel regression model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.plm <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  md <- object$args$model
  ef <- object$args$effect
  y <- pmodel.response(object$model, model = md, effect = ef, theta = object$ercomp$theta)
  sigma <- sqrt(var(object$residuals)) # problem
  den <- dnorm(y, mean = predict(object, newdata = object$frame, model = md, effect = ef, theta = object$ercomp$theta), sd = sigma)
  return(den)
}

#' Fit the density function for a `udiff` model.
#' @param object the fitted model.
#' @param ... other used arguments.
#' @return the density function.
#' @export
fit.den.udiff <- function(object, ...) {
  if (is.null(object$weights)) {
    object$weights <- 1
  }
  if (length(object$xlevels) != 0) {
    for (i in seq_len(length(object$xlevels))) {
      if (length(levels(object$data[[names(object$xlevels[i])]])) !=
          length(object$xlevels[[i]])) {
        object$xlevels[[i]] <- levels(object$data[[names(object$xlevels[i])]])
      }
    }
  }
  #browser()
  theta <- object$coefficients
  Y <- object$model[,1]
  X <- object$model[,2]
  Z <- object$model[,3]
  Y <- vdummy(Y)[,2:length(unique(Y))]
  X <- vdummy(X)[,2:length(unique(X))]
  Z <- vdummy(Z)[,2:length(unique(Z))]
  end1 = ncol(Y)*(ncol(Z)+1)
  end2 = ncol(Y)*ncol(X)
  end3 = ncol(Z)
  #browser()
  W = cbind(matrix(1,nrow(X),1), Z)
  theta.y = theta[1:end1]
  theta.y = matrix(c(theta.y), nrow=ncol(Z)+1, ncol=ncol(Y))
  psi.y = theta[(end1+1):(end1+end2)]
  psi.y = matrix(c(psi.y), nrow=ncol(X), ncol=ncol(Y))
  phi = theta[(end1+end2+1):(end1+end2+end3)]
  phi = matrix(c(phi), nrow=ncol(Z), ncol=1)
  expZ = exp(Z %*% phi) # [nrow * 1]
  sump = W %*% theta.y  + (X %*% psi.y * c(expZ)) # add expZ for each col
  ll = rowSums(Y * sump) - log(1+rowSums(exp(sump)))
  return(-ll)
}

