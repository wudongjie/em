emstep <- function(models, post_pr, n, algo = "em", cfreq = 1, max_iter = 300, 
                   abs_tol = 1e-4, concomitant = NULL, mf.con = NULL, verbose = TRUE) {
  cnt <- 0
  conv <- 1
  llp <- 0
  latent <- length(models)
  while ((abs(conv) > abs_tol) & (max_iter > cnt)) {
    post_pr <- post_pr[rep(seq_len(nrow(post_pr)), cfreq), ]
    if (length(post_pr) == n) {
      post_pr <- matrix(post_pr, ncol = 1)
    }
    pi_matrix <- matrix(colSums(post_pr) / nrow(post_pr),
      nrow = nrow(post_pr), ncol = ncol(post_pr),
      byrow = TRUE
    )

    results <- mstep(models, post_pr = post_pr)

    # Likely that there are not enough obs in a class.
    # browser()
    # if (results$)
    if (length(concomitant) != 0) {
      if ("formula" %in% names(concomitant)) {
        results.con <- mstep.concomitant(concomitant$formula, mf.con, post_pr)
        pi_matrix <- results.con$fitted.values
      } else {
        stop("concomitant need to be a formula")
      }
    }
    pi <- colSums(pi_matrix) / sum(pi_matrix)
    post_pr <- estep(results, pi_matrix)
    if (length(cfreq) != 1) {
      post_pr <- aggregate(post_pr, by = list(rep(seq_len(length(cfreq)), cfreq)), sum)[, -1]
      post_pr <- post_pr / rowSums(post_pr)
    }
    if (algo == "cem") {
      post_pr <- cstep(post_pr)
    } else if (algo == "sem") {
      post_pr <- sstep(post_pr)
    }
    ll <- 0
    if (length(concomitant) == 0) {
      for (i in seq_len(length(results))) {
        if (pi[[i]] != 0) {
          ll <- ll + pi[[i]] * fit.den(results[[i]])
          # ll <- ll + post_pr[,i]*fit.den(results[[i]])
        }
      }
      ll <- sum(log(ll))
    } else {
      for (i in seq_len(length(results))) {
        if (any(!is.na(results[[i]]))) {
          ll <- ll + results.con$fitted.values[, i] * fit.den(results[[i]])
        }
      }
      ll <- sum(log(ll))
    }
    conv <- ll - llp
    llp <- ll
    if (verbose) {
      cat(paste0(
        "Iteration ", cnt, ": ",
        "(EM) log likelihood = ",
        round(ll, 4), "\n"
      ))
    }
    cnt <- cnt + 1
  }
  z <- list(
    models = results,
    pi = colSums(pi_matrix) / sum(pi_matrix),
    latent = latent,
    algorithm = algo,
    obs = n,
    post_pr = estep(results, pi_matrix),
    concomitant = concomitant
  )
  return(z)
}
