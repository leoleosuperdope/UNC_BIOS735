#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec logistic_regression_beta( arma::vec y,arma::mat X){
  double tol = 10^-6;
  double eps = 10000;
  int maxit = 10;
  int m = 0;
  int k=X.n_cols;
  int n=X.n_rows;
  arma::vec beta = arma::zeros(k);
  while (m < maxit && eps > tol){
    arma::vec beta_prev = beta;
    arma::vec px=1/(1+exp(-X*beta));
    arma::mat X_tilde=repelem(px%(1-px),1,k)%X;
    beta=beta+arma::inv(X.t()*X_tilde)*X.t()*(y-px);
    m = m+1;
    eps =sum((beta-beta_prev)%(beta-beta_prev));
  }
  return beta;
}

// [[Rcpp::export]]
arma::vec ridge_logistic_regression_beta2( arma::vec y,arma::mat X, double lambda){
  double tol = 10^-10;
  double eps = 10000;
  int maxit = 10;
  int m = 0;
  int k=X.n_cols;
  int n=X.n_rows;
  arma::vec beta = arma::zeros(k);
  while (m < maxit && eps > tol){
    arma::vec beta_prev = beta;
    arma::vec px=1/(1+exp(-sum(X%repelem(beta,1,n).t(),1)));
    arma::mat X_tilde=repelem(px%(1-px),1,k)%X;
    beta=beta+arma::inv(X.t()*X_tilde+arma::eye(k,k)*lambda)*(X.t()*(y-px)-lambda*beta);
    m = m+1;
    eps =sum((beta-beta_prev)%(beta-beta_prev));
  }
  return beta;
}
