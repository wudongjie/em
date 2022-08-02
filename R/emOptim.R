emOptim <- function(models, pr, algo="em", max_iter=300, verbose=T, sample5=F, cluster.by=NULL, 
                    concomitant=NULL, mf.con=NULL, cfreq=1) {
  cnt <- 0
  conv <- 1
  llp <- 0
  theta <- c()
  if (as.character(models[[1]]$call[[1]]) == "lm") {
    family = "gaussian"
  } else {
    family = models[[1]]$family[1]$family
  }

  if (as.character(models[[1]]$call[[1]]) == "coxph") {
    family = "clogit"
    cl <- models[[1]]$call
    cl$subset <- NULL
    cl[[1L]] <- quote(stats::model.frame)
    cl$method <- NULL
    mf <- eval(cl,  attr(models[[1]]$terms, ".Environment"))
    Y <- model.response(mf)
    Y <- as.matrix(as.double(Y[,2]))
    X <- model.matrix.coxph(models[[1]],data=mf)
    temp <- untangle.specials(models[[1]]$terms, 'strata', 1)
    strat <- as.integer(strata(mf[temp$vars], shortlabel=T))
    X <- as.matrix(cbind(X, strat))
  } else if ("multinom" %in% as.character(models[[1]]$call[[1]])) {
    X <- as.matrix(model.matrix(models[[1]]$terms, models[[1]]$model))
    cl <- models[[1]]$call
    cl$subset <- NULL
    cl[[1L]] <- quote(stats::model.frame)
    cl$method <- NULL
    mf <- eval(cl,  attr(models[[1]]$terms, ".Environment"))
    Y <- as.matrix(vdummy(model.response(mf))[,-1])
    family = "multinom"
    theta = matrix(nrow=length(models))
    for (i in 1:length(models)) {
      theta = cbind(theta, coef(models[[i]]))
    }
    theta = theta[,-1]
    theta = t(theta)
  } 
  else {
    X <- as.matrix(model.matrix(models[[1]]$terms, models[[1]]$model))
    Y <- as.matrix(model.response(models[[1]]$model))
  }
  for (i in 1:length(models)) {
    if (family == "gaussian") {
      theta <- c(theta, 1, coef(models[[i]]))
    } else {
      theta <- c(theta, coef(models[[i]]))
    }
  }
  constraint <- matrix(1)
  if (cfreq != 1) {
    pr <- pr[rep(1:nrow(pr), cfreq),]
  }
  if (sample5) {
    st <- gen_start(theta, pr, ll, gr, Y, X, models, family, constraint, cfreq, algo)
    theta <- st$theta
    pr <- st$pr
  }
  while((abs(conv) > 1e-4) & (max_iter > cnt)) {
    ll <-  partial(mix_ll, d=pr, 
                   Y=Y, X=X,
                   latent=length(models), family=family, isLog=T, constraint=constraint)
    gr <- gen_gr(ll)

    v <- fitoptim(theta, ll, gr)
    theta <- v$par
    if (length(concomitant)!=0) {
      if ("formula" %in% names(concomitant)) {
        results.con <- mstep.concomitant(concomitant$formula, mf.con, pr)
        pi_matrix <- results.con$fitted.values
      } else {
        stop("concomitant need to be a formula")
      }
    } else {
      pi_matrix <- matrix(colSums(pr)/nrow(pr),
                          nrow=nrow(pr), ncol=ncol(pr),
                          byrow=T)
    }
    pr  <- post_pr(theta, pi_matrix, Y, X, length(models), family, constraint=constraint)
    if (length(cfreq) != 1) {
      pr <- aggregate(pr, by=list(rep(1:length(cfreq), cfreq)), sum)[,-1]
      pr <- pr/rowSums(pr)
      pr <- pr[rep(1:nrow(pr), cfreq),]
      pr <- matrix(unlist(pr), ncol=ncol(pr))
    }
    if (algo=="cem")     {
      pr <- cstep(pr)
    }
    else if (algo=="sem") {
      pr <- sstep(pr)
    }
    ll_value <- v$value
    #ll_value <- v$function_value
    conv <- ll_value - llp
    llp <- ll_value
    if (verbose) {
      cat(paste0("Iteration ", cnt, ": ",
                 "(EM) log likelihood = ",
                 round(ll_value, 4), "\n"))
    }
    cnt <- cnt + 1
  }
  if (family == "clogit") {
    linv <- binomial()$linkinv
    X <- X[,-ncol(X)]
  } else if (family == "gaussian") {
    linv <- function (eta) eta
  } 
  else {
    linv <- models[[i]]$family$linkinv
  }
  npar <- length(v$par)/length(models);
  for (i in 1:length(models)) {
    if (family == "gaussian") {
      models[[i]]$coefficients[]<- v$par[((i-1)*npar+2): ((i-1)*npar+npar)]
    } else {
      models[[i]]$coefficients[] <- v$par[((i-1)*npar+1): ((i-1)*npar+npar)]
    }
    models[[i]]$fitted.values <- linv(X %*% models[[i]]$coefficients)
    models[[i]]$residuals <- Y - models[[i]]$fitted.values
    models[[i]]$value <- v$value
    models[[i]]$pi_matrix <- pi_matrix
  }
  return(models)
}


fitoptim <- function(theta, ll, gr) {
  optim(theta, ll, gr, method="BFGS", hessian=T)
  #optim(theta, ll, method="Nelder-Mead", hessian=T)
  #optimization::optim_sa(ll, theta, lower=rep(-100,length(theta)), upper=rep(100, length(theta)))
  #optimization::optim_nm(ll, start=theta)
}

gen_start <- function(theta, pr, ll, gr, Y, X, models, family, constraint, cfreq, algo="em") {
  mll <- c()
  theta <- c(theta)
  theta_list <- list()
  pr_list <- list()
  init_pr <- pr
  npar = length(theta) / 2
  #theta[1:npar] <- runif(npar, -1, 1)
  #theta[(npar+1):length(theta)] <- theta[1:npar] + dnorm(theta[1:npar], 0, 1)
  # e.g. theta <- (-0.5, 0.5, -0.5+dnorm(0,1), 0.5+dnorm(0,1))
  # for (i in 2:length(model)) {
  #   theta[]
  # }
  for (i in 1:5) {
    #theta[1:npar] <- c(models[[1]]$coefficients)
    # co <- -1
    # if (length(models) > 1) {
    #   for (k in 1:length(models))
    #   {
    #     theta[((k-1)*npar+1):((k-1)*npar+npar)] <- theta[1:npar] + rnorm(theta[1:npar],co, 2)
    #     co <- co * -1
    #   }
    # }
    theta <- runif(length(theta), -1, 1)
    pr <- init_pr
    cat(paste0("Random start: ", i, "\n"))
    for (j in 1:5) {
      ll <-  partial(mix_ll, d=pr, 
                     Y=Y, X=X,
                     latent=length(models), family=family, isLog=T, constraint=constraint)
      gr <- gen_gr(ll)
      result <- fitoptim(theta, ll, gr)
      theta <- result$par
      pi_matrix <- matrix(colSums(pr)/nrow(pr),
                          nrow=nrow(pr), ncol=ncol(pr),
                          byrow=T)
      pr  <- post_pr(theta, pi_matrix, Y, X, length(models), family, constraint=constraint)
      if (length(cfreq) != 1) {
        pr <- aggregate(pr, by=list(rep(1:length(cfreq), cfreq)), sum)[,-1]
        pr <- pr/rowSums(pr)
        pr <- pr[rep(1:nrow(pr), cfreq),]
        pr <- matrix(unlist(pr), ncol=ncol(pr))
      }
      if (algo=="cem")     {
        pr <- cstep(pr)
      }
      else if (algo=="sem") {
        pr <- sstep(pr)
      }
      cat(paste0("Iteration ", j, ": ",
                 "(EM) log likelihood = ",
                 round(result$value, 4), "\n"))
    }
    theta_list[[i]] <- theta
    pr_list[[i]] <- pr
    mll <- c(mll, result$value)
  }
  return(list(theta=theta_list[[which.min(mll)]], pr=pr_list[[which.min(mll)]]))
}