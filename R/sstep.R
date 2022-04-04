#' S-step of EM algorithm
#' @description Given the posterior probability, generate a matrix to assign
#' each individual to a class. The assignment is randomly sampled based on the posterior probability.
#' @param postpr (`matrix()`) \cr
#' The matrix of the posterior probability
sstep = function(postpr) {
  assign_func = function(postpr) {
    vec <- rmultinom(1:length(postpr), size=1,prob=postpr)
    return(vec)
  }
  return(t(apply(postpr,1,assign_func)))
}
