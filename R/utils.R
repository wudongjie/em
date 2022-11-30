#' Transform a factor variable to a matrix of dummy variables
#' @param x a factor vector
#' @return a matrix of dummy variables
#' @export
vdummy = function(x) {
  stopifnot( all(x == floor(x)) )
  stopifnot( all(x > 0) )
  m <- max(x)
  to_dummy = function(val) {
    vec <- rep(0, m)
    vec[val] <- 1
    return(vec)
  }
  dvec <- sapply(x, to_dummy)
  if (length(dvec) == length(x)) {
    return(matrix(dvec,ncol=1))
  } else {
    return(t(dvec))
  }
}

#' Flatten a data.frame or matrix by column or row with its name.
#' The name will be transformed into the number of row/column plus 
#' the name of column/row separated by `.`. 
#' @param x a data.frame or matrix.
#' @param by either by column or by row.
#' @return a flattened vector with names 
#' @export
flatten <- function(x, by=c("col","row")){
  by <- match.arg(by)
  if (by == "col") {
    if (is.null(rownames(x))) {
      stop("The matrix to flatten should have a row name!")
    }
    z <- c()
    z.name <- c()
    for (i in (1:ncol(x))) {
        z <- c(z, x[,i])
        na <- paste(i,rownames(x),sep='.')
        z.name <- c(z.name, na)
    }
    names(z) <- z.name
  } else {
    if (is.null(colnames(x))) {
      stop("The matrix to flatten should have a column name!")
    }
    z <- c()
    z.name <- c()
    for (i in (1:nrow(x))) {
      z <- c(z, x[i,])
      na <- paste(i,colnames(x),sep='.')
      z.name <- c(z.name, na)
    }
    names(z) <- z.name
  }
  z
}


partial <- function(f, ...) {
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}

gen_gr = function(ll) {
  gr <- function(theta) {
    g <- numDeriv::grad(ll,theta)
    return(g)
  }
  return(gr)
}

model.matrix.coxph <- utils::getFromNamespace("model.matrix.coxph", "survival")