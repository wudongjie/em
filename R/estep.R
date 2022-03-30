#' @title E-Step of EM Algorithm
#'
#' @author Dongjie Wu
#'
#' @description This function performs an E-Step of EM Algorithm.
#'
#' @return the fitting result for the model.
#'

#estep <- function(models, pi_matrix) UseMethod("estep")

#' @export
estep <- function(models, pi_matrix)
{
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
    for (i in 1: ncol(pi_matrix)) {
      w <- fit.den(models[[i]])*pi_matrix[,i]
        #exp(predict(models[[i]]))
      postpr <- cbind(postpr, w)
    }
    postpr <- matrix(unlist(postpr), ncol=ncol(pi_matrix))
    pr <- postpr / rowSums(postpr)
    return(pr)
}
