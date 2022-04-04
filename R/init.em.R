#' Initialization of EM algorithm
#' @description Given a matrix with number of rows equal to the number of
#' observation and number of columns equal to the number of latent classes,
#' function `init.em` generate the posterior probability using that matrix
#' based on the method set by the user.
#' @return The posterior probability matrix
#' @export
init.em <- function(object, ...) {
  if (!is.matrix(object)) {
    stop("The object should be a matrix!")
  }
  UseMethod("init.em")
}

init.em.random <- function(object, ...) {
  z <- vdummy(sample(1:ncol(object), size=nrow(object), replace=T))
  z
}

init.em.kmeans <- function(object, data, ...) {
  if (!missing(...)) {
    args = list(...)
    z <- vdummy(kmeans(data, centers=ncol(object), args))
  } else {
    z <- vdummy(kmeans(data, centers=ncol(object))$cluster)
  }
  z
}
