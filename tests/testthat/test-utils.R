test_that("test flatten", {
  ### The base line level
  coefMatrix <- matrix(c(0.9, 0.4), ncol = 2)
  colnames(coefMatrix) <- c("(Intercept)", "yb")
  flatten(coefMatrix, by = "row")
  coefMatrix2 <- matrix(c(0.9, 0.4, 0.2, 0.3), ncol = 2)
  colnames(coefMatrix2) <- c("(Intercept)", "yb")
  result <- flatten(coefMatrix2, by = "row")
  exp_result <- c(0.9, 0.2, 0.4, 0.3)
  names(exp_result) <- c("1.(Intercept)", "1.yb", "2.(Intercept)", "2.yb")
  expect_equal(result, exp_result)
})
