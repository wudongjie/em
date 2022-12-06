#' S-step of EM algorithm
#' @description Given the posterior probability, generate a matrix to assign
#' each individual to a class. The assignment is randomly sampled based on the posterior probability.
#' @param postpr (`matrix()`) \cr
#' The matrix of the posterior probability
sstep <- function(postpr) {
  assign_func <- function(postpr) {
    vec <- rmultinom(seq_len(length(postpr)), size = 1, prob = postpr)
    return(vec)
  }
  y <- apply(postpr, 1, assign_func)
  if (ncol(postpr) == 1) {
    return(matrix(y, ncol = 1))
  } else {
    return(t(y))
  }
}
