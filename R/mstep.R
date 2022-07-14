#' M-Step of EM algorithm
#' @description This function performs an M-Step of EM Algorithm.
#' @param models the models used in the EM algorithm
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
    # The corner case: all post_pr is 0 for one class
    result <- list()
    cls <- list()
    upost_pr <- unique(as.vector(post_pr))
    env <- parent.frame(3)
    for (i in 1:length(models)) {
    if (isS4(models[[i]])) {
      cls[[i]] <- models[[i]]@call
    } else {
      cls[[i]] <- models[[i]]$call
    } 
    }
    
    if ((length(upost_pr) <= 2) && (1 %in% upost_pr)) {
      # Do k estimations separately
      for (i in 1:length(models)) {
        cl <- cls[[i]]
        cl$subset <- ( post_pr[, i] == 1)
        if (sum(cl$subset) == 0) {
          result[[i]] <- NA
        } else {
          result[[i]] <- suppressWarnings(eval(cl, env))
          #result[[i]]$cfreq <- models[[i]]$cfreq
          #browser()
          #print(result[[i]]$coefficients)
          if ("glmerMod" %in% class(result[[i]])) {
            result[[i]]@frame = models[[i]]@frame
          } else { 
            result[[i]]$model <- models[[i]]$model
          }
        }
      }
    } else {
      for (i in 1:length(models)) {
          #browser()
          cl <- cls[[i]]
          wts <- post_pr[, i]
          cl$weights <- wts
          result[[i]] <- suppressWarnings(eval(cl, env))
          #result[[i]]$cfreq <- models[[i]]$cfreq
          #result[[i]]$model <- models[[i]]$model
      }
    }
    return(result)
}
