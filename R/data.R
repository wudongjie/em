#' @title Simulated Regression Data
#'
#' @description A data set with simulated data from mixture regression models.
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{yp}{A dependent variable generaged from a mixture of a poisson regression with x}
#'   \item{yn}{A dependent variable generaged from a mixture of a linear regression with x}
#'   \item{yc}{A dependent variable generaged from a mixture of a linear regression with x and a concomitant variable of z}
#'   \item{x}{An independent variable}
#'   \item{z}{A concomitant variable}
#' }
#' @source <https://www.github.com/wudongjie/em>
"simReg"

#' @title Simulated Data from a logistic regression
#'
#' @description A data set with simulated data from a mixture of a logistic regression.
#'
#' @format A data frame with 10000 rows and 2 variables:
#' \describe{
#'   \item{y}{A dependent variable generaged from a mixture of a logistic regression with x}
#'   \item{x}{An independent variable}
#' }
#' @source <https://www.github.com/wudongjie/em>
"simBinom"

#' @title Simulated Data from a conditional logistic regression
#'
#' @description A data set with simulated data from a mixture of a conditional logistic regression.
#'
#' @format A data frame with 10000 rows and 4 variables:
#' \describe{
#'   \item{y1}{A dependent variable generaged from a conditional logistic regression with x2 and x3}
#'   \item{y2}{A dependent variable generaged from a mixture of two classes of conditional logistic regressions with x2 and x3}
#'   \item{x2}{A dummy variable showing whether x is equal to level 2}
#'   \item{x3}{A dummy variable showing whether x is equal to level 3}
#' }
#' @source <https://www.github.com/wudongjie/em>
"simClogit"