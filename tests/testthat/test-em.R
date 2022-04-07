library(gnm)

test_that("test linear regression", {
  browser()
  NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  formula2 <- yn~x
  fit_lm <- lm(formula, data=NPreg)
  pd <- predict(fit_lm)
  results <- em(fit_lm, latent=2, verbose=T)
  # Test predict
  fmm_fit <- predict(results)
  fmm_fit_post <- predict(results, prob="posterior")

  # Test cem and sem
  #results_cem <- cem(lm, formula=formula, data=NPreg, latent=2, verbose=T)
  #results_sem <- sem(lm, formula=formula, data=NPreg, latent=2, verbose=T)
  #print(summary(results_cem))
  #print(summary(results_sem))
  
  
  print(summary(results))
  results2 <- em(fit_lm, init.method="kmeans") # Test kmeans
  print(summary(results2))
  # Test update
  results4 <- update(results, latent=3)
  glm_fit <- glm(formula=formula, data=NPreg)
  results_glm <- em(glm_fit)
  print(summary(results_glm))
 
})

test_that("test concomitant", {
  NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn ~ x + x2
  formula_c <- ~ yb
  lm_fit <- lm(formula, data=NPreg)
  results <- em(lm_fit, concomitant=list(formula=formula_c, data=NPreg), verbose=T)
  fmm_fit <- predict(results)
  print(summary(results))
  results3 <- update(results, latent=3)
  print(summary(results3))
  glm_fit <- glm(formula, data=NPreg)
  results_glm <- em(glm_fit, concomitant=list(formula=formula_c, data=NPreg))
  print(summary(results_glm))
})


test_that("test glm poisson", {
  NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
  formula2 <- yp~x
  fit_glm <- glm(formula2, family=poisson, data=NPreg)
  print(fit_glm)
  results2 <- em(fit_glm, latent=2)
  print(summary(results2))
})

test_that("test glm logit", {
  # Example from "https://rdrr.io/cran/mixtools/man/logisregmixEM.html"
  set.seed(100)
  beta <- matrix(c(-10, .1, 20, -.1), 2, 2)
  x <- runif(500, 50, 250)
  x1 <- cbind(1, x)
  xbeta <- x1%*%beta
  w <- rbinom(500, 1, .3)
  y <- w*rbinom(500, size = 1, prob = (1/(1+exp(-xbeta[, 1]))))+
    (1-w)*rbinom(500, size = 1, prob =
                   (1/(1+exp(-xbeta[, 2]))))
  dt <- data.frame(y=y, x=x)
  formula <- y~x
  fit_glm <- glm(formula=formula, family=binomial, data=dt)
  fit_em <- em(fit_glm, latent=2, verbose = T)
  print(summary(fit_em))
 })


test_that("test gnm poisson(unidiff)", {
  ## Example from Turner and Firth (2020)
  #browser()
  #
  ### Collapse to 7 by 7 table as in Erikson et al. (1982)
  erikson <- as.data.frame(gnm::erikson)
  lvl <- levels(erikson$origin)
  levels(erikson$origin) <- levels(erikson$destination) <-
      c(rep(paste(lvl[1:2], collapse = " + "), 2), lvl[3],
          rep(paste(lvl[4:5], collapse = " + "), 2), lvl[6:9])
  erikson <- xtabs(Freq ~ origin + destination + country, data = erikson)
  levelMatrix <- matrix(c(2, 3, 4, 6, 5, 6, 6,
                          3, 3, 4, 6, 4, 5, 6,
                          4, 4, 2, 5, 5, 5, 5,
                          6, 6, 5, 1, 6, 5, 2,
                          4, 4, 5, 6, 3, 4, 5,
                          5, 4, 5, 5, 3, 3, 5,
                          6, 6, 5, 3, 5, 4, 1), 7, 7, byrow = TRUE)
  formula1 <- Freq ~ country:origin + country:destination
  formula2 <- Freq ~ country:origin + country:destination + Topo(origin, destination, spec = levelMatrix)
  formula3 <- Freq ~ country:origin + country:destination + Mult(Exp(country),
                            Topo(origin, destination, spec = levelMatrix))
  formula4 <- Freq ~ country:origin + country:destination + country:Topo(origin, destination, spec = levelMatrix)

  udf1 <- gnm(formula=formula1, family=poisson, data=erikson)
  udf2 <- gnm(formula=formula2, family=poisson, data=erikson)
  udf3 <- gnm(formula=formula3, family=poisson, data=erikson)
  udf4 <- gnm(formula=formula4, family=poisson, data=erikson)
  udf2_1 <- em(udf1, latent=2)
  ## Interaction specified by levelMatrix, common to all countries
  udf2_2 <- em(udf2, latent=2)
  #udf2_3 <- em(udf3, latent=2)
  udf2_3 <- multi.em(udf3, latent=2)
  udf2_4 <- em(udf4, latent=2)

  print(summary(udf2_1))
  print(summary(udf2_2))
  print(summary(udf2_3))
  print(summary(udf2_4))
})

# 
# test_that("test clogit", {
#   library(survival)
#   browser()
#   resp <- levels(logan$occupation)
#   n <- nrow(logan)
#   indx <- rep(1:n, length(resp))
#   logan2 <- data.frame(logan[indx,],
#                        id = indx,
#                        tocc = factor(rep(resp, each=n)))
#   logan2$case <- (logan2$occupation == logan2$tocc)
#   formula1 <- case ~ tocc + tocc:education + strata(id)
#   cl_fit <- clogit(formula1, logan2)
#   cl_wfit <- clogit(formula1, logan2, weights=rep(1, 838*5), method="approximate")
#   cl_fit2 <- em(cl_fit, latent=2)
#   
#   cl <- cl_fit$call
#   cl[[1L]] <- quote(stats::model.frame)
#   cl$method <- NULL
#   mf <- eval(cl)#, parent.frame())
#   y <- model.response(mf)
#   y <- as.double(y[,2])
#   fitted <- exp(predict(cl_fit))
#   dt <- data.frame(y=y, fitted=fitted)
#   gen.mn <- function(dt) {
#     dmultinom(dt$y, prob=dt$fitted)
#   }
#   #id=mf$`strata(id)`
#   results <- unlist(by(dt, mf$`strata(id)`, gen.mn, simplify = F))
#   browser()
# })