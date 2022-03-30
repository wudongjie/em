test_that("test m-step", {
  x1 <- rnorm(1000, mean=0, sd=1)
  x2 <- rnorm(1000, mean=0, sd=1)
  theta <- matrix(c(1,2,5,1,4,3), ncol=1)
  pi_v <- matrix(c(0.36, 0.64), ncol=100, nrow=2)
  pi_v <- t(pi_v)
  Y <- matrix(1, nrow=1000, ncol=1)
  Y[1:360] <- theta[2,1]*x1[1:360] + theta[3,1]*x2[1:360]
  Y[361:1000] <- theta[5,1]*x1[361:1000]+theta[6,1]*x2[361:1000]
  X <- matrix(c(x1,x2),ncol = 2)
  latent <- 2
  formula <- Y ~ 0 + x1 + x2
  data <- data.frame(Y=Y, x1=x1, x2=x2)
  oD <- runif(1000, 0, 1)
  oD <- ifelse(oD>=0.55, 1, 0)
  oDm <- matrix(c(1-oD, oD), ncol=2)
  D <- rep(1,1000)
  D[361:1000] <- 0
  rD <- 1 - D
  mD <- matrix(c(D, rD), ncol=2)
  data <- data.frame(Y=Y,x1=x1,x2=x2)
  pars <- list(formula=formula, data=data)
  browser()
  y_result <- mstep(list(lm, lm), pars, oDm)
  pi_matrix <- matrix(colSums(oDm)/nrow(oDm),
                      nrow=1000, ncol=2,
                      byrow=T)
  mD <- estep(y_result, pi_matrix)
  y_result <- mstep(list(lm, lm), pars, mD)
  pi_matrix <- matrix(colSums(mD)/nrow(mD),
                      nrow=1000, ncol=2,
                      byrow=T)
  mD2 <- estep(y_result, pi_matrix)
  y_result <- mstep(list(lm, lm), pars, mD2)
  print(y_result)
  #sum_to_1 = rep(c(1),100)
  #expect_equal(rowSums(y_result), sum_to_1)
})
