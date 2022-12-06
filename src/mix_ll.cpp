//
//  mix_ll.cpp
//  Rcpp_test
//
//  Created by Dongjie Wu on 15/11/2021.
//
#include <iostream>
#include <RcppArmadillo.h>
#include "Family.h"
#include "gen_theta.h"

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

template <class T>
double family_mixer(const arma::vec& theta, const arma::mat& Y,
                                     const arma::mat& X, const arma::mat& d,
                                     int latent, bool isLog){
    Family<T>* f = new T;
    int npar = theta.n_elem / latent;
    //int p = X.n_cols;
    //int n = X.n_rows;
    double l = 0.0;
    arma::rowvec pi_vector = arma::sum(d, 0)/d.n_rows;
    arma::mat result = arma::zeros(Y.n_rows, 1);

    if (isLog) {
      for (int i=0; i<latent;i++) {
        arma::vec theta_l = theta.subvec((npar*i), (npar*i+npar-1));
        arma::mat pdist = f->logLik(theta_l, Y, X, true);
        arma::mat pi_v = arma::mat(pdist.n_rows,pdist.n_cols,
                                   arma::fill::value(log(pi_vector[i])));
        l = l + accu(d(arma::span(0,(d.n_rows-1)),i) % (pdist+pi_v));

      }
    } else {
      arma::mat result = arma::zeros(Y.n_rows, 1);
      for (int i=0; i<latent;i++) {
        arma::vec theta_l = theta.subvec((npar*i), (npar*i+npar-1));
        arma::mat pdist = f->logLik(theta_l, Y, X, false);
        arma::mat pi_v = arma::mat(pdist.n_rows,pdist.n_cols,
                                   arma::fill::value(pi_vector[i]));
        result = result + pi_v % pdist;
      }
      l = accu(arma::log(result));
    }
      l = -l;
    delete f;
    return l;
}



// [[Rcpp::export]]
NumericVector mix_ll(const arma::vec& theta, const arma::mat& Y,
                     const arma::mat& X, const arma::mat& d,
                     unsigned int latent, Rcpp::CharacterVector family, bool isLog,
                     const arma::mat& constraint) {
    double l = 0.0;
    // d.n_cols = latent
    if (d.n_cols != latent) {
        throw std::invalid_argument("d matrix should have 'latent' columns!");
    }


    // set and check family
    std::string fam = Rcpp::as<std::string>(family[0]);
    
    // Y is multicolumn if family is multinom.
    if ((fam == "multinom") && (Y.n_cols<2)) {
        throw std::invalid_argument("Y is not a multi-column variable!");
    }
    
    if (fam == "gaussian") {
        // theta_size = Y.ncols * X.ncols * latent
        if (theta.size() != Y.n_cols * (X.n_cols+1)*latent) {
            throw std::invalid_argument("Wrong numbers of estimates!");
        }
        l = family_mixer<FamilyNormal>(theta, Y,
                                   X, d, latent, isLog);
    } else if (fam == "poisson") {
        if (theta.size() != Y.n_cols * X.n_cols*latent) {
            throw std::invalid_argument("Wrong numbers of estimates!");
        }
        l = family_mixer<FamilyPoisson>(theta, Y,
                                    X, d, latent, isLog);
    } else if (fam == "logit" || fam == "binomial"){
        if (theta.size() != Y.n_cols * X.n_cols*latent) {
            throw std::invalid_argument("Wrong numbers of estimates!");
        }
        l = family_mixer<FamilyLogit>(theta, Y,
                                  X, d, latent, isLog);
    } else if (fam == "clogit"){
        arma::vec theta_t = arma::vec((X.n_cols-1)*latent, arma::fill::zeros);
        if (constraint.size() != 1) {
            theta_t = gen_theta(theta, constraint);
        } else {
            theta_t = theta;
        }
        l = family_mixer<FamilyConditionalLogit>(theta_t, Y,
                                  X, d, latent, isLog);
    } else if (fam == "multinom") {
        if ((theta.size() != Y.n_cols * X.n_cols*latent) && (constraint.size() == 1)) {
            throw std::invalid_argument("Wrong numbers of estimates!");
        }
        arma::vec theta_t = arma::vec(Y.n_cols * X.n_cols*latent, arma::fill::zeros);
        if (constraint.size() != 1) {
            theta_t = gen_theta(theta, constraint);
        } else {
            theta_t = theta;
        }
        l = family_mixer<FamilyMultiNomial>(theta_t, Y,
                                        X, d, latent, isLog);
    } else if (fam == "unidiff") {
        l = family_mixer<FamilyUnidiff>(theta, Y,
                                        X, d, latent, isLog);
    }
    else {
        throw std::invalid_argument( "Family does not exist!" );
    }
  
    if (std::isnan(l)) {
        return NumericVector::create(R_NaN);
    } else if (std::isinf(l)) {
        if (l>0) {
            return NumericVector::create(R_PosInf);
    } else {
        return NumericVector::create(R_NegInf);
    }
  }
    else {
        return NumericVector::create(l);
    }
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
x1 <- runif(100, 0, 1)
x2 <- runif(100, 0, 1)
X <- matrix(data = c(x1,x2), ncol=2)
y <- runif(100)
theta <- c(1,5,3,1,2,4)
y[1:30] <- 5*x1[1:30] + 3*x2[1:30]
y[31:100] <- 2*x1[31:100] + 4*x2[31:100]
y <- matrix(y, ncol=1)
d1 <- matrix(1, nrow=100, ncol=1)
d1[31:100] <- 0
d2 <- matrix(1, nrow=100, ncol=1)
d2[1:30] <- 0
d <- cbind(d1,d2)
family <- c("gaussian")
result <- mix_ll(theta, y, X, d, 2, family)
print(result)
*/

