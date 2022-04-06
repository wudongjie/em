#' Multiple run of EM algorithm
multi.em <- function(object, ...) {
  UseMethod("multi.em")
}



#' Default generic for multi.em
#' @param object the model to use in em, e.g. `lm`, `glm`, `gnm`
#' @param ... arguments used in em.
#' @param iter number of iterations for running EM algorithm.
#' @param parallel whether to use the parallel computing.
#' @return return the `em` object with the maximum log-likelihood.
#' @export
multi.em.default <- function(object, iter=10, parallel=FALSE, ...)
{
  args <- list(...)
  args$object <- object
  fitted <- list()
  j <- 1
  if (parallel) {
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        numCores <- 2L
      } else {
        # use all cores in devtools::test()
        numCores <- parallel::detectCores()
      }
      fitted <- parallel::mclapply(seq_len(iter),
                                   function(x) {
                                     tryCatch({
                                       do.call(em, args)
                                     }, error=function(e) {
                                       NULL
                                     }
                                     )
                                   }, mc.cores = numCores
      )
  } else {
      fitted <- lapply(seq_len(iter),
                   function(x) {
                      tryCatch({
                         do.call(em, args)
                         }, error=function(e) {
                            NULL
                         }
                         )
                   }
    )
  }

  maxid = 1
  fitted<-fitted[!sapply(fitted,is.null)]
  if (length(fitted) == 0) {
    stop("Fit failed!")
  } else if (length(fitted) == 1) {
    maxid <- 1
  } else {
    for (i in (2:length(fitted))) {
      if (logLik(fitted[[i]]) > logLik(fitted[[maxid]])) {
        maxid <- i
      }
    }
  }
  cat(paste0("Pick iteration ", maxid))
  return(fitted[[maxid]])
}
