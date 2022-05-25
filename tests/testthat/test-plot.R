test_that("Test Plots", {
    NPreg <- read.csv(list.files(system.file('extdata', package = 'em'), full.names = T)[1])
    NPreg$x2 <- (NPreg$x)^2
    formula <- yn~x+x2
    formula2 <- yn~x
    fit_lm <- lm(formula, data=NPreg)
    browser()
    glm_fit <- glm(formula=formula, data=NPreg)
    pd <- predict(fit_lm)
    results <- em(fit_lm, latent=3, verbose=T)
    plot(results)
    dev.off()
    plot(results, by="response")
    dev.off()
    plot(results, by="prob")
    dev.off()
    plot(results, by="prob.hist")
    dev.off()
})
