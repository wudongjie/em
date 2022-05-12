# test_that("multiplication works", {
#   library(gnm)
#   library(parallel)
#   erikson <- as.data.frame(gnm::erikson)
#   lvl <- levels(erikson$origin)
#   levels(erikson$origin) <- levels(erikson$destination) <-
#     c(rep(paste(lvl[1:2], collapse = " + "), 2), lvl[3],
#       rep(paste(lvl[4:5], collapse = " + "), 2), lvl[6:9])
#   erikson <- xtabs(Freq ~ origin + destination + country, data = erikson)
#   levelMatrix <- matrix(c(2, 3, 4, 6, 5, 6, 6,
#                           3, 3, 4, 6, 4, 5, 6,
#                           4, 4, 2, 5, 5, 5, 5,
#                           6, 6, 5, 1, 6, 5, 2,
#                           4, 4, 5, 6, 3, 4, 5,
#                           5, 4, 5, 5, 3, 3, 5,
#                           6, 6, 5, 3, 5, 4, 1), 7, 7, byrow = TRUE)
#   formula1 <- Freq ~ country:origin + country:destination + Mult(Exp(country),
#                                                                  Topo(origin, destination, spec = levelMatrix))
#   gnm_fit <- gnm(formula=formula1, family=poisson, data=erikson)
#   #browser()
#   # t1 <- system.time(
#   # udf_np <- multi.em(gnm_fit, latent=2)
#   # )
#   t2 <- system.time(
#   udf_p <- multi.em(gnm_fit, latent=2, parallel=T)
#   )
#   print(t1)
#   print(t2)
# })
