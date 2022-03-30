# test_that("test fitting mixture of distribution", {
#   browser()
#   y <- rnorm(200,1,2)
#   y[101:200] <- rnorm(100,3,2)
#   args <- list(data=y,distr="norm")
#   fitd1 <- do.call(fitdistrplus::fitdist, args)
#   print(summary(fitd1))
#   mixtools::normalmixEM(y)
#
#   fitd2 <- cem(fitdistrplus::fitdist, args, latent=2)
#   print(summary(fitd2))
#   browser()
#
# })
