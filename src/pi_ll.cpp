//  pi_ll.cpp
//  Rcpp_test
//
//  Created by Dongjie Wu on 20/1/2022.
//
#include <iostream>
#include <RcppArmadillo.h>
#include "Family.hpp"
#include "gen_theta.hpp"

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericVector pi_ll(const arma::vec& alpha,
              const arma::mat& X, const arma::mat& d, int latent){
    arma::mat alpha_t = arma::mat(alpha);
    alpha_t.reshape(X.n_cols, d.n_cols);
    arma::mat pdist = arma::exp(X * alpha_t);
    arma::mat sum_col = arma::sum(pdist, 1);
    sum_col = arma::join_rows(sum_col, sum_col);
    pdist = pdist/sum_col;
    double l = 0.0;
    l = accu(d % arma::log(pdist));
    return NumericVector::create(l);
}
