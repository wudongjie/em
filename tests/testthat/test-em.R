test_that("test linear regression", {
  browser()
  NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  formula2 <- yn~x
  fit_lm <- lm(formula, data=NPreg)
  glm_fit <- glm(formula=formula, data=NPreg)
  pd <- predict(fit_lm)
  result1 <- em(fit_lm, latent=1)
  results <- em(fit_lm, latent=2, verbose=T)
  # Test predict
  fmm_fit <- predict(results)
  fmm_fit_post <- predict(results, prob="posterior")

  #Test cem and sem
  results_sem <- em(fit_lm, latent=2, verbose=T, algo="sem")
  # cem is very likely to result in all variables assigned to one class
  results_cem <- multi.em(fit_lm, latent=2, verbose=T, algo="cem")
  print(summary(results_cem))
  print(summary(results_sem))
  print(predict(results_sem))


  print(summary(results))
  results2 <- em(fit_lm, init.method="kmeans") # Test kmeans
  print(summary(results2))
  # Test update
  results4 <- update(results, latent=3)
  results_glm <- em(glm_fit)
  print(summary(results_glm))

})

test_that("test concomitant", {
  browser()
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
  browser()
  NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
  formula2 <- yp~x
  fit_glm <- glm(formula2, family=poisson, data=NPreg)
  print(fit_glm)
  results2 <- em(fit_glm, latent=2)
  print(summary(results2))
})

test_that("test glm logit", {
  # Example from "https://rdrr.io/cran/mixtools/man/logisregmixEM.html"
  browser()
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
  library(gnm)

  ## Example from Turner and Firth (2020)
  browser()
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

test_that("test clogit", {
  library(survival)
  browser()
  usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[2])
  usdata_ex <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])

  usdata$in1 <- as.factor(usdata$x == usdata$y)
  usdata$a1 <- as.factor((usdata$x == 1) & (usdata$y == 1))
  usdata$a2 <- as.factor((usdata$x == 2) & (usdata$y == 2))
  usdata$a3 <- as.factor((usdata$x == 3) & (usdata$y == 3))
  usdata$y <- as.factor(usdata$y)
  usdata$x <- as.factor(usdata$x)
  usdata_ex$in1 <- as.factor((usdata_ex$a1_x1==1) | (usdata_ex$a2_x2==1) | (usdata_ex$a3_x3==1))
  formula1 <- chosen ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3  + strata(obs)
  formula1.in1 <- chosen ~ 0 + a2 + a3 + in1 + strata(obs)
  formula1.in1.f <- chosen ~ 0 + a2 + a3 + in1 + strata(obs)
  formula2 <- obs ~ x + y + in1
  formula3 <- obs ~ x + y + a1 + a2 + a3
  p_fit <- glm(formula2, family=poisson, data=usdata)

  browser()
  #Fix the predict of missing coeffieicnets
  #p_fit4 <- em(p_fit, latent=5)

  cl_fit <- clogit(formula1.in1, usdata_ex)
  cl_fit1 <- em(cl_fit, latent=1, algo="sem", verbose=T)
  #cl_wfit <- clogit(formula1, logan2, weights=rep(1, 838*5), method="breslow")
  p_fit2 <- em(p_fit, latent=2)
  cl_fit2 <- em(cl_fit, latent=2, algo="sem")
  print(summary(cl_fit2))
  browser()
})



# test_that("plm", {
#   browser()
#   library(plm)
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
#   formula1 <- chosen ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3
#   pfit_fe <- plm(formula1, data=usdata,
#               model="within",
#               index=c("obs", "alt"))
#   print(summary(pfit_fe))
#   pfit_re <- plm(formula1, data=usdata,
#                 model="random",
#                 index=c("obs", "alt"))
#   print(summary(pfit_re))
#   pfit_em <- em(pfit_fe, latent=2, algo="em")
#   print(summary(pfit_em))
#   pfit_em_r <- em(pfit_re, latent=2)
#   print(summary(pfit_em_r))
#   })


# test_that("pglm", {
#   browser()
#   library(pglm)
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
#   formula1 <- as.factor(chosen) ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3
#   pfit_fe <- pglm(formula1, data=usdata, family=list(family="binomial",link="logit") ,
#                  effect="individual",model="random",
#                  index=c("obs", "alt"))
#   print(summary(pfit_fe))
#   pfit_re <- pglm(formula1, data=usdata, family="binomial",
#                  model="random",
#                  index=c("obs", "alt"))
#   print(summary(pfit_re))
# })

# test_that("glmer", {
#   browser()
#   library(lme4)
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
#   formula1 <- chosen ~ a2 + a3 + a1_x1 + a2_x2 + a3_x3 + (1 | obs)
#   fit_me <- glmer(formula1, data=usdata, family="binomial")
#   print(summary(fit_me))
#   fmm_me <- em(fit_me, latent=2)
#   print(summary(fmm_me))
# })

# test_that("fitdist", {
#   browser()
#   library(fitdistrplus)
#   y <- runif(200,0,1)
#   y[1:100] <- rpois(100, 3)
#   y[101:200] <- rpois(100,2)
#   d_fit <- fitdist(y, "pois")
#   em_fit <- em(d_fit, latent=2)
#   print(summary(em_fit))
# })


