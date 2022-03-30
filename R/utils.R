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
  return(t(dvec))
}


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
