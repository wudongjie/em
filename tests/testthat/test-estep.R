test_that("test e-step", {
  x1 <- rnorm(100, mean = 1, sd = 1)
  x2 <- rnorm(100, mean = 1, sd = 1)
  theta <- matrix(c(1, 1, 2, 1, 2, 3), ncol = 1)
  pi_v <- matrix(c(0.36, 0.64), nrow = 2, ncol = 100)
  pi_v <- t(pi_v)
  Y <- matrix(1, nrow = 100, ncol = 1)
  Y[1:36] <- theta[2, 1] * x1[1:36] + theta[3, 1] * x2[1:36]
  Y[37:100] <- theta[5, 1] * x1[37:100] + theta[6, 1] * x2[37:100]
  X <- matrix(c(x1, x2), ncol = 2)
  latent <- 2
  formula <- Y ~ 0 + x1 + x2
  D <- runif(100, 0, 1)
  rD <- 1 - D
  data <- data.frame(Y = Y, x1 = x1, x2 = x2)
  model1 <- lm(formula, data, weights = D)
  model2 <- lm(formula, data, weights = rD)
  y_result <- estep(list(model1, model2), pi_v)
  sum_to_1 <- rep(c(1), 100)
  expect_equal(rowSums(y_result), sum_to_1)
})
