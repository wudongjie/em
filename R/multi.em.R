#' @export
multi.em <- function(object, ..., iter=10, parallel=FALSE)
{
  args <- list(...)
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
                                       do.call(object, args)
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
                         do.call(object, args)
                         }, error=function(e) {
                            NULL
                         }
                         )
                   }
    )
  }

  maxid = 1
  fitted<-fitted[!sapply(fitted,is.null)]
  for (i in (2:length(fitted))) {
    if (logLik(fitted[[i]]) > logLik(fitted[[maxid]])) {
      maxid <- i
    }
  }
  cat(paste0("Pick iteration ", maxid))
  return(fitted[[maxid]])
}
