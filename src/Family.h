//
//  Family.hpp
//  test_rcpp
//
//  Created by Dongjie Wu on 18/11/2021.
//

#ifndef Family_hpp
#define Family_hpp

#include <stdio.h>
#include <RcppArmadillo.h>
//#include <boost/math/distributions/normal.hpp>
//#include <boost/multiprecision/cpp_bin_float.hpp>
//#include "distribution.hpp"
//using namespace boost::math;
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


// !! need to decide provide the option of not log
template <class T>
class Family {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false) {
    return(static_cast<T*>(this) -> logLik(theta, Y, X, lg));
  }
  
};

class FamilyNormal : public Family<FamilyNormal> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_mat = (X * theta.subvec(1,(theta.n_elem-1)));
    double sd = sqrt(theta[0] * theta[0]);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;++i) {
      for (int j=0; j<Y.n_cols;++j) {
          //normal_distribution nd(mean_mat.at(i,j),sd);
          l.at(i,j) = R::dnorm4(Y.at(i,j), mean_mat.at(i,j), sd, lg);
      }
    }
    return l;
  }
};

class FamilyPoisson : public Family<FamilyPoisson> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_mat = arma::exp(X * theta); // the precision problem
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);

    for (int i=0; i<Y.n_rows;++i) {
      for (int j=0; j<Y.n_cols;++j) {
          l.at(i,j) = R::dpois(Y.at(i,j), mean_mat.at(i,j), lg);
      }
    }
    return l;
  }
};

class FamilyLogit : public Family<FamilyLogit> {
public:
  arma::mat sigmoid(arma::mat x) {
          return (1/(1+arma::exp(-x)));
      };
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
//      arma::mat mean_t = (X * theta);
//      arma::mat mean_mat = log(sigmoid(mean_t));
//      arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
//      for (int i=0; i<Y.n_rows;i++) {
//        for (int j=0; j<Y.n_cols; j++) {
//           l.at(i,j) = R::dbinom(Y.at(i,j), 1, mean_mat.at(i,j), lg);
//        }
//      }
//      return l;
    arma::mat mean_t = (X * theta);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    l = (Y % mean_t) - arma::log1p(arma::exp(mean_t));
    return l;
  }
};

class FamilyMultiNomial : public Family<FamilyMultiNomial> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat theta_t = arma::mat(theta);
    theta_t.reshape(X.n_cols, Y.n_cols);
    arma::mat mean_mat = (X * theta_t);
    arma::vec r = arma::sum((Y % mean_mat), 1) - log(1 + arma::sum(arma::exp(mean_mat), 1));
    arma::mat l = arma::mat(r);
    return l;
  }
};

class FamilyConditionalLogit : public Family<FamilyConditionalLogit> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
 // The group variable is the last variable of X.
  {
    //auto t1 = std::chrono::high_resolution_clock::now();
    arma::uvec gV = arma::conv_to<arma::uvec>::from(X.col(X.n_cols-1));
    arma::uvec g = unique(gV);
    arma::mat l = arma::zeros(g.n_elem, 1);
    arma::mat l1 = arma::zeros(g.n_elem, 1);
    arma::mat l2 = arma::zeros(g.n_elem, 1);
    //auto t2 = std::chrono::high_resolution_clock::now();
    for (arma::uword i=0, nr=Y.n_rows; i<nr; ++i) {
        arma::uword j = X.at(i,X.n_cols-1)-1;
        double mean_x = 0;
        for (arma::uword k=0, nc=X.n_cols; k<nc-1; ++k) {
            mean_x += theta.at(k) * X.at(i, k);
        }
        l1.at(j, 0) += Y.at(i, 0) * mean_x;
        l2.at(j, 0) += exp(mean_x);
    }
    //auto t3 = std::chrono::high_resolution_clock::now();
    l = l1 - log(l2);
    //auto t4 = std::chrono::high_resolution_clock::now();

    /*
    const arma::mat& Xd = X.cols(0, X.n_cols-2);
    auto t2 = std::chrono::high_resolution_clock::now();

    arma::cube Xc1(Y.n_rows, Y.n_cols, 1);
    Xc1.slice(0) = Y % (Xd * theta);
    Xc1.reshape(gV.n_elem/g.n_elem, Y.n_cols, g.n_elem);
    arma::cube Xc2(Y.n_rows, Y.n_cols, 1);
    Xc2.slice(0) = arma::exp(Xd * theta);
    Xc2.reshape(gV.n_elem/g.n_elem, Y.n_cols, g.n_elem);
    auto t3 = std::chrono::high_resolution_clock::now();

    for (arma::uword i=0; i<g.n_elem;++i) {
      l.at(i, 0) = arma::accu(Xc1.slice(i)) -     log(arma::accu(Xc2.slice(i)));
    }
    auto t4 = std::chrono::high_resolution_clock::now();
    
    std::chrono::duration<double, std::milli> d1 = t2 - t1;
    std::chrono::duration<double, std::milli> d2 = t3 - t2;
    std::chrono::duration<double, std::milli> d3 = t4 - t3;
    std::cout << d1.count() << "ms" << std::endl;
    std::cout << d2.count() << "ms" << std::endl;
    std::cout << d3.count() << "ms" << std::endl;
     */
    return l;
  }
};

class FamilyUnidiff : public Family<FamilyUnidiff> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
 // The group variable is the last variable of X.
    {
        if (Y.n_cols != 1 ) {
            throw std::invalid_argument("Y should only have 1 column!");
        }
        if (X.n_cols != 2 ) {
            throw std::invalid_argument("X should have 2 columns!");
        }
        arma::vec uY = arma::unique(Y);
        arma::mat X_X = fast_dummy(X.col(0));
        arma::mat X_Z = fast_dummy(X.col(1));
        arma::mat Y_Y = fast_dummy(Y.col(0));
        arma::mat W = arma::ones(X_Z.n_rows, (X_Z.n_cols+1));
        W(arma::span::all,arma::span(1,W.n_cols-1)) = X_Z;
        arma::uword colX = X_X.n_cols;
        arma::uword colZ = X_Z.n_cols;
        arma::uword colY = Y_Y.n_cols;
        if (theta.n_elem != (colY*(colZ+1)+colY*colX+colZ)) {
            throw std::invalid_argument("Wrong size of theta!");
        }
        arma::uword end = colY*(colZ+1)-1;
        arma::uword end2 = colY*colX;
        arma::uword end3 = colZ;
        arma::mat theta_y = arma::mat(theta(arma::span(0, end)));
        theta_y.reshape(colZ+1, colY);
        arma::mat psi_y = arma::mat(theta(arma::span(end+1, end+end2)));
        psi_y.reshape(colX, colY);
        arma::mat phi = arma::mat(theta(arma::span(end+end2+1,end+end2+end3)));
        phi.reshape(colZ,1);
        arma::mat expZ = arma::exp(X_Z * phi);
        arma::mat phiX = X_X  * psi_y;
        arma::mat part2 = phiX.each_col() % expZ;
        arma::mat sum_part = W * theta_y + part2;
        arma::mat first = arma::sum(Y_Y % sum_part, 1);
        arma::mat second = log(1+arma::sum(arma::exp(sum_part), 1));
        arma::mat l = first - second;
        return l;
    }
    arma::mat fast_dummy(const arma::vec& x) {
        arma::vec vec_u = arma::unique(x);
        vec_u = vec_u(arma::span(1,vec_u.n_elem-1)); // Remove the baseline
        arma::mat mats = arma::zeros(x.n_elem, (vec_u.n_elem));
        for (arma::uword i=0; i<vec_u.n_elem; ++i) {
            mats.col(i) = x;
            double idx = vec_u.at(i);
            mats.col(i).for_each([idx](arma::vec::elem_type& val){
                (val == idx) ? (val = 1) : (val = 0);
            });
        }
        return mats;
    }
};



#endif /* Family_hpp */
