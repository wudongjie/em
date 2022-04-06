#' M-Step of EM algorithm
#' @description This function performs an M-Step of EM Algorithm.
#' @param models the models used in the EM algorithm
#' @param pars the arguments in the models
#' @param post_pr the posterior probability.
#' @return the fitting result for the model.
#' @export
mstep <- function(models, post_pr=NULL)
{
    if (!is.list(models)) {
      stop("Please provide models in a list.")
    }
    if (!is.null(post_pr)) {
      if(!is.matrix(post_pr)) {
        stop("post_pr is not a matrix.")
      }
      if (length(models) != ncol(post_pr)) {
        stop("The number of fitted models is not equal to the number of columns of post_pr!")
      }
    }
    result <- list()
    for (i in 1:length(models)) {
        cl <- models[[i]]$call
        cl$weights <- post_pr[, i]
        env <- attr(models[[i]]$terms, ".Environment")
        result[[i]] <- suppressWarnings(eval(cl, env))
    }
    return(result)
}
