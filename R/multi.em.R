#' Multiple run of EM algorithm
#' @export
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
multi.em.default <- function(object, iter=10, parallel=FALSE, random.init=TRUE, ...)
{
  args <- list(...)
  args$object <- object
  if (is.null(args$latent)) {
    args$latent = 2
  }
  rcluster <- function(x){
    if ( (!is.vector(x)) | length(x) == 0 ) {
      stop("init.prob is not a vector")
    }
    if (length(x) == 1) {
      return(c(1))
    } else {
      y <- sapply(x, function(y){rlnorm(1, 0, 1)})
      y[-1] <- exp(1)
      return(y)
    }
  }
  init.prob = matrix(1, nrow=iter, ncol=args$latent)
  if (random.init) {
    init.prob = t(apply(init.prob, 1, rcluster))
  }
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
                                       do.call(em, args, envir = parent.frame(3))
                                       
                                       
                                     }, error=function(e) {
                                       message(str(args))
                                       #print(message(ls(parent.frame(3))))
                                       #print(message(e))
                                       NULL
                                     }
                                     )
                                   }, mc.cores = numCores
      )
  } else {
      #browser()
      fitted <- apply(init.prob, 1,
                   function(x) {
                      tryCatch({
                         do.call(em, args=append(args, list(init.prob=x)), envir = parent.frame(3))
                         }, error=function(e) {
                            NULL
                         }
                         )
                   }
    )
  }
  #browser()
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
