#' Test the fitted density function "fit.den.R"
#' `logLik(object)` should be equal to `sum(log(fit.den(object)))`
test_that("test lm glm", {
    formula <- yn~x
    formula2 <- yp~x
    fit_lm <- lm(formula, data=simReg)
    fit_glm <- glm(formula=formula, data=simReg)
    fit_glm_p <- glm(formula2, family=poisson, data=simReg)
    print(logLik(fit_lm))
    print(sum(log(fit.den(fit_lm))))
    expect_equal(logLik(fit_lm)[[1]], sum(log(fit.den(fit_lm))), tolerance=1e-3)
    expect_equal(logLik(fit_glm)[[1]], sum(log(fit.den(fit_glm))), tolerance=1e-3)
    expect_equal(logLik(fit_glm_p)[[1]], sum(log(fit.den(fit_glm_p))))
})

test_that("test gnm", {})

# test_that("panelmodel", {
#   browser()
#   library(plm)
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
#   formula1 <- chosen ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3
#   pfit_fe <- plm(formula1, data=usdata,
#                  model="within",
#                  index=c("obs", "alt"))
#   expect_equal(logLik(pfit_fe)[[1]], sum(log(fit.den(pfit_fe))))
#   pfit_re <- plm(formula1, data=usdata,
#                  model="random",
#                  index=c("obs", "alt"))
#   expect_equal(logLik(pfit_re)[[1]], sum(log(fit.den(pfit_re))))
# })

test_that("test clogit", {})