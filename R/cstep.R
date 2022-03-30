#' @description Given the posterior probability, generate a matrix to assign
#' each individual to a class. The assignment based on which probability is the largest.
#' @param postpr (`matrix()`) \cr
#' The matrix of the posterior probability
cstep = function(postpr) {
  assign_func = function(postpr) {
    vec <- rep(0,length(postpr))
    vec[[which.max(postpr)]] <- 1
    return(vec)
  }
  return(t(apply(postpr,1,assign_func)))
}
