

nobs.fitdist <- function(object, ...) {
  object$n
}

coef.summary.fitdist <- function(object, ...) {
  z <- cbind(object$estimate, object$sd)
  colnames(z) <- c("coef", "sd")
  z
}
