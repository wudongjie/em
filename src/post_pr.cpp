//
//  post_pr.cpp
//  test_rcpp
//
//  Created by Dongjie Wu on 03/12/2021.
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
arma::mat pp_mixer(const arma::vec& theta, const arma::mat& pi_m,
                const arma::mat& Y, const arma::mat& X, const int& latent){
  Family<T>* f = new T;
  int npar = theta.n_elem / latent;
  //int p = X.n_cols;
  //int n = X.n_rows;
  //arma::mat pi_m = arma::diagmat(pi_v);
  arma::mat z_m = arma::zeros(pi_m.n_rows, latent);
  arma::mat theta_t = arma::mat(theta);
  for (int i=0; i<latent;i++) {
      arma::vec theta_l = theta.subvec((npar*i), (npar*i+npar-1));
      arma::mat pdist = f->logLik(theta_l, Y, X, true);
      //Rcpp::Rcout << pdist << std::endl;
      z_m.col(i) = arma::exp(pdist);
      //Rcpp::Rcout << z_m << std::endl;
  }
  arma::mat z_sum = arma::zeros(z_m.n_rows, z_m.n_cols);
  arma::mat z_w = z_m%pi_m;
  for (int i=0; i<latent;i++) {
      z_sum.col(i) = arma::sum(z_w, 1);
  }
  arma::mat z = z_w/z_sum;
  delete f;
  return z;
}




// [[Rcpp::export]]
arma::mat post_pr(const arma::vec& theta, const arma::mat& pi_m,
                      const arma::mat& Y, const arma::mat& X, const int& latent,
                      Rcpp::CharacterVector family, const arma::mat& constraint) {
    arma::mat z;

    // set and check family
    std::string fam = Rcpp::as<std::string>(family[0]);
    
    // Y is multicolumn if family is multinom.
    if ((fam == "multinom") && (Y.n_cols<2)) {
        throw std::invalid_argument("Y is not a multi-column variable!");
    }
    
    if (fam == "gaussian") {
        z = pp_mixer<FamilyNormal>(theta, pi_m, Y,
                                       X, latent);
    } else if (fam == "poisson") {
        z = pp_mixer<FamilyPoisson>(theta, pi_m, Y,
                                        X, latent);
    } else if (fam == "logit" || fam == "binomial"){
        z = pp_mixer<FamilyLogit>(theta, pi_m, Y,
                                      X, latent);
    } else if (fam == "clogit"){
        z = pp_mixer<FamilyConditionalLogit>(theta, pi_m, Y,
                                      X, latent);
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
        z = pp_mixer<FamilyMultiNomial>(theta_t, pi_m, Y,
                                            X, latent);
    } else if (fam == "unidiff") {
        z = pp_mixer<FamilyUnidiff>(theta, pi_m, Y,
                                        X, latent);
    } else {
        throw std::invalid_argument( "Family does not exist!" );
    }
    return z;
}
