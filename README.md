# em
A generic function in R to implement EM algorithm for finite mixture models (FMM).

## Introduction

Our `em` package follows R's feature of generic functions and the function `em()` can be implemented after a model fitting with one component using R's pre-existing
functions and packages such as `glm()`, `lm()`, and so on.

Currently, it supports the following models: linear models (`lm()`), generalized linear models (`glm()`), generalized non-linear model (`gnm()`), survival models (mainly conditional logistic regression) (`survival::clogit()`), multinomial models(`nnet::multinom()`).

em also makes use of other generic functions in R for post-estimations and results printing.

To use these functions, one can just provide the object of estimation results from em() as an input. Example: `summary(emfit)` summarise the results from `em()`.

The supported generic functions are: `summary()`, `print()`, `predict()`, `update()`, `logLik()`, `df()`, `plot()`.


## Installation

Install and test this ongoing package from Github:

``` r
remotes::install_github("wudongjie/em")
```

## Example
### Use `em` to Fit Finite Mixture of Linear Models:

```
formula2 <- yn ~ x
fit_lm <- lm(formula, data=NPreg)
results <- em(fit_lm, latent=2, verbose=T)
print(summary(results))
```

### Use `em` to Fit Finite Mixture of Logistic Regression Models:

```
formula <- y ~ x
fit_glm <- glm(formula=formula, family=binomial, data=dt)
fit_em <- em(fit_glm, latent=2, verbose = T, init.method = "kmeans", use.optim=T)
print(summary(fit_em))
```

