#' C-Step of EM algorithm
#' @description Given the posterior probability, generate a matrix to assign
#' each individual to a class. The assignment is based on which probability is the largest.
#' @param postpr (`matrix()`) \cr
#' The matrix of the posterior probability
cstep = function(postpr) {
  assign_func = function(postpr) {
    vec <- rep(0,length(postpr))
    vec[[which.max(postpr)]] <- 1
    return(vec)
  }
  if (ncol(postpr) == 1) {
    return(matrix(rep(1, nrow(postpr)),ncol=1))
  } else {
    return(t(apply(postpr,1,assign_func)))
  }
}
