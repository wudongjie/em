## This section was inspired by Flexmix.

mstep.concomitant <- function(formula, data, postpr) {
  x <- model.matrix(formula, data)
  y <- postpr
  r <- ncol(x)
  p <- ncol(y)
  mask <- c(rep(0, r + 1), rep(c(0, rep(1, r)), p - 1))
  w <- rep(1, nrow(y))
  if ((!nrow(y)) | (nrow(y)<2)) {
    stop("Posterior probabilities should be a matrix with at least two columns")
  }
  nnet::nnet.default(x, y, w, mask = mask, size = 0,
               skip = TRUE, softmax = TRUE, censored = FALSE,
               rang = 0, trace=FALSE)
}

mstep.concomitant.refit <- function(formula, data, postpr) {
  x <- model.matrix(formula, data)
  y <- postpr
  w <- rep(1, nrow(y))
  rownames(y) <- rownames(x) <- NULL
  fit <- nnet::multinom(y ~ 0 + x, weights = w, data = list(y = y, x = x), Hess = TRUE, trace = FALSE)
  fit$coefnames <- colnames(x)
  fit$vcoefnames <- fit$coefnames[seq_along(fit$coefnames)]
  dimnames(fit$Hessian) <- lapply(dim(fit$Hessian) / ncol(x), function(i) paste(rep(seq_len(i) + 1, each = ncol(x)), colnames(x), sep = ":"))
  fit
}

