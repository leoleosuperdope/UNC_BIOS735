#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector one_or_exp(NumericVector x) {
  NumericVector y(x);
  y = ifelse(x < 0.0, 1.0, exp(x));
  return y;
}

// [[Rcpp::export]]
List randomWalk2Rcpp(double niter, double lambda) {
  NumericVector x(niter);
  x[0] = 0;
  for (int i=1; i < niter; ++i) {
    x[i] = x[i-1] + lambda*(2.0 * Rf_rbinom(1, 0.5) - 1.0);
  }
  NumericVector y(niter);
  y[0] = 0;
  for (int i=1; i < niter; ++i) {
    y[i] = y[i-1] + lambda*(2.0 * Rf_rbinom(1, 0.5) - 1.0);
  }
  List z = List::create(Named("x") = x, _["y"] = y );
  return z ;
}

// [[Rcpp::export]]
arma::mat armadillo_solve(arma::mat A, arma::vec b) {
  int m = A.n_cols;
  arma::mat Z(m,1);
  Z = solve(A, b);
  return Z;
}

// [[Rcpp::export]]
arma::mat col_ridge_2(arma::mat Y, arma::mat X, arma::vec lambda) {
  int m = X.n_cols;
  int n = Y.n_cols;
  arma::mat ID = arma::eye<arma::mat>(m, m);
  arma::mat Betahat(m, n);
  for (int i=0; i < n; ++i) {
   Betahat.col(i) = (X.t() * X + lambda[i] * ID).i() * X.t() * Y.col(i);
  }
  return Betahat;
}





