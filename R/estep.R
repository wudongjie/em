#' This function performs an E-Step of EM Algorithm.
#' @param models models used in the EM algorithm,
#' @param pi_matrix the pi matrix.
#' @return the fitting result for the model.
#' @export
estep <- function(models, pi_matrix) {
  if (!is.matrix(pi_matrix)) {
    stop("pi_matrix is not a matrix.")
  }
  if (!is.list(models)) {
    stop("Please provide models in a list.")
  }
  if (length(models) != ncol(pi_matrix)) {
    stop("The number of fitted models is not equal to the number of columns of pi_matrix!")
  }
  postpr <- list()
  for (i in seq_len(ncol(pi_matrix))) {
    if (sum(pi_matrix[, i]) == 0) {
      w <- pi_matrix[, i]
    } else {
      w <- fit.den(models[[i]]) * pi_matrix[, i]
    }
    # exp(predict(models[[i]]))
    postpr <- cbind(postpr, w)
  }
  postpr <- matrix(unlist(postpr), ncol = ncol(pi_matrix))
  pr <- postpr / rowSums(postpr)
  return(pr)
}
