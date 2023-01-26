# test_that("test udiff", {
#   library(udiff)
#   usdata_ex <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
#   #usdata_ex$y <- as.factor(usdata_ex$y)
#   #usdata_ex$x <- as.factor(usdata_ex$x)
#   #usdata_ex$g <- as.factor(usdata_ex$g)
#   #browser()
#   formula1 <- y ~ x + g
#   result <- udiff(formula1, usdata_ex)
#   browser()
#   print(summary(result))
#   fmm2 <- em::em(result, latent=2, algo="sem", verbose=TRUE, max_iter=30)
#   print(summary(fmm2))
# })
