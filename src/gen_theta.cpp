//
//  gen_theta.cpp
//  test_rcpp
//
//  Created by Dongjie Wu on 07/12/2021.
//
#include <iostream>
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

arma::vec gen_theta(const arma::vec& theta, const arma::mat& constraint) {
    // check constraint matrix, the last index of theta should be the max value of constraint
    
    arma::vec out = arma::vec(constraint.size());
    int count = 0;
    for (const auto& val : constraint) {
        out[count] = theta[val];
        count = count + 1;
    }
    return out;
}
