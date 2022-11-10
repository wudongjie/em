#' Plot the fitted results of EM algorithm
#' @description This is the generic plot function for `em` project. One can produce
#' three types of graphs using this function 
#' 1. A graph of the predicted value distribution for each component.
#' 2. A histogram of posterior probability distributions
#' @param x the `em` model to plot
#' @param by the type of the graph to produce. The default is `component`. 
#' @param prior whether fit the model using prior probabilities.
#' @param cols lines' colors.
#' @param lwds Lines' widths.
#' @param ltys lines' types.
#' @param ranges the ranges of the x-axis and the y-axis limits of plots. 
#' It should be a vector of four numeric values. The first two represent the x-axis limits.
#' The last two represent the y-axis limits 
#' @param main the main title.
#' @param lgd a list for legend related arguments. 
#' @param lgd.loc the location of the legend. The default is "topleft".
#' @param hist.args The list of arguments for the histogram.
#' @param ... other arguments.
#' @importFrom graphics legend lines hist par title
#' @export
plot.em <- function(x, by=c("component", "prob"), prior=F, 
                    cols=rep(1, length(x$models)), 
                    lwds=rep(3, length(x$models)), 
                    ltys=c(1:length(x$models)), 
                    ranges=NULL, main=NULL, lgd=list(), lgd.loc="topleft",
                    hist.args=list(main="Histograms of posterior probabilities",
                                   xlab="Posterior Probabilities"), ...) {
  if (!is.null(ranges)) {
    if (length(ranges) != 4) {
      stop("Please set correct ranges with 4 elements.")
    }
  }
  t <- match.arg(by)
  prob <- NA
  if (prior) {
    prob <- "prior"
  } else {
    prob <- "posterior"
  }
  fitted <- predict(x, prob=prob)
  if (t == "component") {
    if (is.null(main)) {
      main = "The distribution of the fitted value by component"
    }
    if (is.null(ranges)) {
      ranges <-  apply(data.frame(fitted$components), 2,
                       function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
    }
    plot(density(fitted$components[[1]]), 
         col=cols[[1]],
         lwd=lwds[[1]],
         lty=ltys[[1]], xlim = range(ranges[1:2, ]),
         ylim = range(ranges[3:4, ]),
         main = main)
    if (length(fitted$components) > 1) {
      for (i in (1:length(fitted$components))){
          lines(density(fitted$components[[i]]),
                col=cols[[i]],
                lwd=lwds[[i]],
                lty=ltys[[i]])
      }
    }
    lgd[[1L]] <- lgd.loc
    lgd$legend <-  sapply(1:length(fitted$components), function(x){paste("Comp.", x)})
    lgd$col <- cols
    lgd$lwd <- lwds
    lgd$lty <- ltys
    do.call(legend, lgd)  
  }
  # else if (t == "response") {
  #   if (is.null(main)) {
  #     main = "The distribution of the fitted value vs. the observed value"
  #   }
  #   y <- model.response(x$models[[1]]$model)
  #   if (is.null(ranges)) {
  #     ranges <-  apply(data.frame(fitted=fitted$mean, y=y), 2,
  #                      function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  #   }
  #   plot(density(fitted$mean), 
  #        col=cols[[1]],
  #        lwd=lwds[[1]],
  #        lty=ltys[[1]], xlim = range(ranges[1:2, ]),
  #        ylim = range(ranges[3:4, ]),
  #        main = main)
  #   lines(density(y),
  #         col=cols[[2]],
  #         lwd=lwds[[2]],
  #         lty=ltys[[2]])
  #   lgd[[1L]] <- lgd.loc
  #   lgd$legend <-  c("Fitted", "Observed")
  #   lgd$col <- cols
  #   lgd$lwd <- lwds
  #   lgd$lty <- ltys
  #   do.call(legend, lgd)  
  # }
  # else if (t == "prob") {
  #   if (is.null(main)){
  #     main = "The distribution of posterior probabilities"
  #   }
  #   for (i in (1:length(x$models))) {
  #     if (is.null(ranges)) {
  #       ranges <-  apply(x$post_pr, 2,
  #                        function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  #     }
  #     if (i == 1) {
  #       plot(density(x$post_pr[,i]), 
  #            col=cols[[1]],
  #            lwd=lwds[[1]],
  #            lty=ltys[[1]], xlim = range(ranges[1:2, ]),
  #            ylim = range(ranges[3:4, ]),
  #            main = main)
  #     } else {
  #       lines(density(x$post_pr[,i]),
  #             col=cols[[i]],
  #             lwd=lwds[[i]],
  #             lty=ltys[[i]])
  #     }
  #   }
  #   lgd[[1L]] <- lgd.loc
  #   lgd$legend <-  sapply(1:length(x$models), function(x){paste("Comp.", x)})
  #   lgd$col <- cols
  #   lgd$lwd <- lwds
  #   lgd$lty <- ltys
  #   do.call(legend, lgd) 
  # }
  else if (t == "prob") {
    par(mfrow=c(3,1)) 
    ttl <- hist.args$main
    for (i in (1:length(x$models))) {
      hist.args$main <- ""
      main <- paste("Comp", i, sep=".")
      hist.args$x <- x$post_pr[,i]
      do.call(hist, hist.args)
      if (i==1) {
        title(ttl, line=2)
      }
      title(main, adj=1)
    }
  }
  else {
    stop("The graph type does not exist.")
  }
}
