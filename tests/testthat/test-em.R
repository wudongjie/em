# test_that("test linear regression", {
#   NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   formula2 <- yn~x
#   fit_lm <- lm(formula, data=NPreg)
#   glm_fit <- glm(formula=formula, data=NPreg)
#   pd <- predict(fit_lm)
#   browser()
#   ##result1 <- em(fit_lm, latent=1)
#   results <- em(fit_lm, latent=2, verbose=T)
#   emfit1 <- em(glm_fit, latent=2, verbose=T, init.method="kmeans", use.optim=T, optim.start="sample5")
#   browser()
#   print(summary(emfit1))
#   # Test predict
#   fmm_fit <- predict(results)
#   fmm_fit_post <- predict(results, prob="posterior")
#   #Test cem and sem
#   results_sem <- em(fit_lm, latent=2, verbose=T, algo="sem")
#   # cem is very likely to result in all variables assigned to one class
#   results_cem <- multi.em(fit_lm, latent=2, verbose=T, algo="cem")
#   print(summary(results_cem))
#   print(summary(results_sem))
# 
# 
#   print(summary(results))
#   browser()
#   results2 <- em(fit_lm, init.method="kmeans", verbose=T) # Test kmeans
#   print(summary(results2))
#   results3 <- em(fit_lm, init.method="hc", verbose=T) # Test kmeans
#   print(summary(results3))
#   # Test update
#   results4 <- update(results, latent=3)
#   results_glm <- em(glm_fit)
#   print(summary(results_glm))
# 
#  })
# test_that("test linear regression", {
#   NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   formula2 <- yn~x
#   fit_lm <- lm(formula, data=NPreg)
#   glm_fit <- glm(formula=formula, data=NPreg)
#   pd <- predict(fit_lm)
#   result1 <- em(fit_lm, latent=1)
#   results <- em(fit_lm, latent=2, verbose=T)
#   # Test predict
#   fmm_fit <- predict(results)
#   fmm_fit_post <- predict(results, prob="posterior")
#   browser()
#   #Test cem and sem
#   results_wem <- em(fit_lm, latent=2, verbose=T, algo="wem")
#   print(summary(results_wem))
#   results_sem <- em(fit_lm, latent=2, verbose=T, algo="sem")
#   # cem is very likely to result in all variables assigned to one class
#   results_cem <- multi.em(fit_lm, latent=2, verbose=T, algo="cem")
#   print(summary(results_cem))
#   print(predict(results_sem))
# 
# 
#   print(summary(results))
#   browser()
#   results2 <- em(fit_lm, init.method="kmeans", verbose=T) # Test kmeans
#   print(summary(results2))
#   results3 <- em(fit_lm, init.method="hc", verbose=T) # Test kmeans
#   print(summary(results3))
#   # Test update
#   results4 <- update(results, latent=3)
#   results_glm <- em(glm_fit)
#   print(summary(results_glm))
# 
# })

# test_that("test concomitant", {
#   browser()
#   NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn ~ x + x2
#   formula_c <- ~ yb
#   lm_fit <- lm(formula, data=NPreg)
#   results <- em(lm_fit, concomitant=list(formula=formula_c, data=NPreg), verbose=T)
#   emfit1 <- em(lm_fit, latent=2, verbose=T, init.method="random", use.optim=T, optim.start="sample5", 
#                concomitant=list(formula=formula_c, data=NPreg))
#   browser()
#   fmm_fit <- predict(results)
#   print(summary(results))
#   results3 <- update(results, latent=3)
#   print(summary(results3))
#   glm_fit <- glm(formula, data=NPreg)
#   results_glm <- em(glm_fit, concomitant=list(formula=formula_c, data=NPreg))
#   print(summary(results_glm))
# })
# 
# 
# test_that("test glm poisson", {
#   browser()
#   NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
#   formula2 <- yp~x
#   fit_glm <- glm(formula2, family=poisson, data=NPreg)
#   print(fit_glm)
#   results2 <- em(fit_glm, latent=2)
#   print(summary(results2))
#   results3 <- em(fit_glm, init.method="kmeans", verbose=T) # Test kmeans
#   print(summary(results3))
#   results4 <- em(fit_glm, init.method="hc", verbose=T) # Test kmeans
#   print(summary(results4))
#   emfit1 <- em(fit_glm, latent=2, verbose=T, init.method="kmeans", use.optim=T, optim.start="sample5")
#   browser()
#   print(summary(emfit1))
# })

# test_that("test glm logit", {
#   # Example from "https://rdrr.io/cran/mixtools/man/logisregmixEM.html"
#   browser()
#   set.seed(100)
#   beta <- matrix(c(-10, .1, 20, -.1), 2, 2)
#   x <- runif(10000, 50, 250)
#   x1 <- cbind(1, x)
#   xbeta <- x1%*%beta
#   w <- rbinom(10000, 1, .3)
#   y <- w*rbinom(10000, size = 1, prob = (1/(1+exp(-xbeta[, 1]))))+
#     (1-w)*rbinom(10000, size = 1, prob =
#                    (1/(1+exp(-xbeta[, 2]))))
#   dt <- data.frame(y=y, x=x)
#   formula <- y~x
#   dt$z <- as.vector(sapply(1:2500, rep, times=4))
#   fit_glm <- glm(formula=formula, family=binomial, data=dt)
#   fit_em <- em(fit_glm, latent=2, verbose = T, init.method = "kmeans", use.optim=T)
#   fit_em2 <- em(fit_glm, latent=2, verbose = T, init.method = "kmeans",
#                use.optim=T, optim.start="sample5")
# 
#   print(summary(fit_em))
#   print(summary(fit_em2))
#   browser()
#  })


# test_that("test gnm poisson(unidiff)", {
#   library(gnm)
# 
#   ## Example from Turner and Firth (2020)
#   browser()
#   #
#   ### Collapse to 7 by 7 table as in Erikson et al. (1982)
#   erikson <- as.data.frame(gnm::erikson)
#   lvl <- levels(erikson$origin)
#   levels(erikson$origin) <- levels(erikson$destination) <-
#       c(rep(paste(lvl[1:2], collapse = " + "), 2), lvl[3],
#           rep(paste(lvl[4:5], collapse = " + "), 2), lvl[6:9])
#   erikson <- xtabs(Freq ~ origin + destination + country, data = erikson)
#   levelMatrix <- matrix(c(2, 3, 4, 6, 5, 6, 6,
#                           3, 3, 4, 6, 4, 5, 6,
#                           4, 4, 2, 5, 5, 5, 5,
#                           6, 6, 5, 1, 6, 5, 2,
#                           4, 4, 5, 6, 3, 4, 5,
#                           5, 4, 5, 5, 3, 3, 5,
#                           6, 6, 5, 3, 5, 4, 1), 7, 7, byrow = TRUE)
#   formula1 <- Freq ~ country:origin + country:destination
#   formula2 <- Freq ~ country:origin + country:destination + Topo(origin, destination, spec = levelMatrix)
#   formula3 <- Freq ~ country:origin + country:destination + Mult(Exp(country),
#                             Topo(origin, destination, spec = levelMatrix))
#   formula4 <- Freq ~ country:origin + country:destination + country:Topo(origin, destination, spec = levelMatrix)
# 
#   udf1 <- gnm(formula=formula1, family=poisson, data=erikson)
#   udf2 <- gnm(formula=formula2, family=poisson, data=erikson)
#   udf3 <- gnm(formula=formula3, family=poisson, data=erikson)
#   udf4 <- gnm(formula=formula4, family=poisson, data=erikson)
#   udf2_1 <- em(udf1, latent=2)
#   ## Interaction specified by levelMatrix, common to all countries
#   udf2_2 <- em(udf2, latent=2)
#   #udf2_3 <- em(udf3, latent=2)
#   udf2_3 <- multi.em(udf3, latent=2)
#   udf2_4 <- em(udf4, latent=2)
# 
#   print(summary(udf2_1))
#   print(summary(udf2_2))
#   print(summary(udf2_3))
#   print(summary(udf2_4))
# })


test_that("test clogit with simulation", {
  browser()
  library(survival)
  set.seed(122)
  beta1 <- matrix(c(2.1,4.1, 8.1, -.1), 2, 2)
  beta2 <- matrix(c(20.3,1.9, 10.5, 0.7), 2, 2)
  x <- sample.int(3, 10000, replace =T)
  x <- vdummy(x)
  z <- runif(10000)
  gamma1 <- matrix(c(0.2, 0.8), nrow=2)
  zbeta <- cbind(1, z) %*% gamma1
  x2 <- x[,2]
  x3 <- x[,3]
  Xt <- cbind(x2, x3)
  xbeta1 <- exp(Xt%*%beta1)
  xbeta2 <- exp(Xt%*%beta2)
  prb1 <- cbind(1, xbeta1)
  prb2 <- cbind(1, xbeta2)
  w <- rbinom(10000, 1, .3)
  # w <- rbinom(10000, size = 1, prob = (1/(1+exp(-zbeta))))
  # One class case
  y1 <- t(apply(prb1, 1, rmultinom, n = 1, size = 1))
  # Two classes
  y2 <- w*t(apply(prb1, 1, rmultinom, n = 1, size = 1))+
    (1-w)*t(apply(prb2, 1, rmultinom, n = 1, size = 1))
  df <- cbind.data.frame(y1=apply(y1, 1, function(x) which(x==1)),
                         y2=apply(y2, 1, function(x) which(x==1)),
                         x2=x2, x3=x3)
  formula_nom1 <- y1 ~ 0 + x2 +x3
  formula_nom2 <- y2 ~ 0 + x2 +x3
  nfit1 <- nnet::multinom(formula_nom1, df)
  nfit2 <- nnet::multinom(formula_nom2, df)
  print(summary(nfit1))
  browser()
  #mfit <- em(nfit2, latent=2, verbose=T, use.optim=T,init.method="kmeans") #init.method="kmeans")
  # extend to clogit form
  y1x <- as.vector(t(y1))
  y2x <- as.vector(t(y2))
  x2x <- as.vector(sapply(x2, rep, times=3))
  x3x <- as.vector(sapply(x3, rep, times=3))
  a2x <- rep(c(0,1,0), 10000)
  a3x <- rep(c(0,0,1), 10000)
  idx <- as.vector(sapply(1:10000, rep, times=3))
  fid <- idx %/% 3 + 1
  dat <- data.frame(chosen1=y1x, chosen2=y2x, x2=x2x, x3=x3x, a2=a2x, a3=a3x, id=idx, fid=fid)
  dat$a2_x2 <- as.integer((dat$a2==1) & (dat$x2==1))
  dat$a2_x3 <- as.integer((dat$a2==1) & (dat$x3==1))
  dat$a3_x2 <- as.integer((dat$a3==1) & (dat$x2==1))
  dat$a3_x3 <- as.integer((dat$a3==1) & (dat$x3==1))
  dat$z <- as.vector(sapply(z, rep, times=3))
  #write.csv(dat, "sim_clogit.csv")
  formula1 <- chosen1 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3+ strata(id)
  formula2 <- chosen2 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3+ strata(id)
  formula3 <- chosen2 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3
  formula_c <- ~ z
  cfit1 <- clogit(formula1, dat)
  print(summary(cfit1))
  browser()
  #emfit1 <- em(cfit1, latent=1, algo="sem")
  #print(summary(emfit1))
  #flexfit1 <- flexmix(cbind(chosen2, 1-chosen2) ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3 | id, data=dat,
  #                    k=2, model = FLXMRglm(family = "binomial"))
  cfit2 <- clogit(formula2, dat)
  #emfit <- em(cfit2, latent=2, algo="sem", verbose=T)
  browser()
  # emfitr <- em(cfit2, latent=2, algo="sem", verbose=T, max_iter=30)
  # print(summary(emfitr))
  # emfitr2 <- em(cfit2, latent=2, algo="sem", init.method="kmeans", verbose=T, max_iter=30)
  # print(summary(emfitr2))
  # emfit <- em(cfit2, latent=2, verbose=T, init.method="kmeans", use.optim=T)
  # print(summary(emfit))
  # emfit2 <- em(cfit2, latent=2, verbose=T, init.method="random", use.optim=T, optim.start="sample5")
  # print(summary(emfit2))
  emfit3 <- em(cfit2, latent=2, verbose=T, init.method="kmeans", use.optim=T, optim.start="sample5")
  print(summary(emfit3))
  # browser()
  #emfit4 <- em(cfit2, latent=2, verbose=T, init.method="kmeans", use.optim=T, algo="sem", optim.start="sample5", cluster.by=dat$fid)
  #print(summary(emfit4))
  emfit_con <- em(cfit2, latent=2, algo="sem", verbose=T, max_iter=30, concomitant=list(formula=formula_c, data=dat))
  print(summary(emfit_con))
  emfit_con2 <- em(cfit2, latent=2, verbose=T, init.method="kmeans", use.optim=T, optim.start="sample5", concomitant=list(formula=formula_c, data=dat))
  print(summary(emfit3))
  # emfit <- em(cfit2, latent=2, init.method="hc", algo="sem", verbose=T, max_iter=100)
  emfit3 <- em(cfit2, latent=2, algo="sem", verbose=T, init.prob=c(0.3,0.7), max_iter=100)
  print(summary(emfit3))
  #mtfit <- multi.em(cfit2, iter=10, random.init=TRUE, latent=2, verbose=T, max_iter=100)
  #print(summary(cfit2))
  browser()
})


# test_that("test clogit", {
#   library(survival)
#   browser()
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[2])
#   usdata_ex <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
# 
#   usdata$in1 <- as.factor(usdata$x == usdata$y)
#   usdata$a1 <- as.factor((usdata$x == 1) & (usdata$y == 1))
#   usdata$a2 <- as.factor((usdata$x == 2) & (usdata$y == 2))
#   usdata$a3 <- as.factor((usdata$x == 3) & (usdata$y == 3))
#   usdata$y <- as.factor(usdata$y)
#   usdata$x <- as.factor(usdata$x)
#   usdata_ex$in1 <- as.factor((usdata_ex$a1_x1==1) | (usdata_ex$a2_x2==1) | (usdata_ex$a3_x3==1))
#   formula1 <- chosen ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3  + strata(obs)
#   formula1.in1 <- chosen ~ 0 + a2 + a3 + in1 + strata(obs)
#   formula1.in1.f <- chosen ~ 0 + a2 + a3 + in1 + strata(obs)
#   formula2 <- obs ~ x + y + in1
#   formula3 <- obs ~ x + y + a1 + a2 + a3
#   p_fit <- glm(formula2, family=poisson, data=usdata)
# 
#   browser()
#   #Fix the predict of missing coeffieicnets
#   #p_fit4 <- em(p_fit, latent=5)
#   cl_fit <- clogit(formula1.in1, usdata_ex)
#   
#   #cl_fit2 <- em(cl_fit, latent=2, algo="sem", verbose=T)
#   
#   cl_fit1 <- em(cl_fit, latent=1, algo="sem", verbose=T)
#   #cl_fit3 <- em(cl_fit, latent=2, algo="sem", verbose=T, cluster.by=usdata_ex$g)
#   #cl_wfit <- clogit(formula1, logan2, weights=rep(1, 838*5), method="breslow")
#   p_fit2 <- em(p_fit, latent=2)
#   print(summary(cl_fit2))
#   browser()
# })


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
# 
# test_that("glmer", {
#   browser()
#   library(lme4)
#   usdata <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[3])
#   formula1 <- chosen ~ a2 + a3 + a1_x1 + a2_x2 + a3_x3 + (1 | obs)
#   fit_me <- glmer(formula1, data=usdata, family="binomial")
#   print(summary(fit_me))
#   fmm_me <- em(fit_me, latent=2, verbose=T,init.method="random.weights", max_iter=30)
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


