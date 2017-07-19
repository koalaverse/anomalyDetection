// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat compute_md(arma::mat x) {
  arma::rowvec colmeans = arma::mean(x, 0);
  arma::mat xsweep = x.each_row() - colmeans;
  return sum(xsweep * arma::pinv(arma::cov(x)) % xsweep, 1);
}


// [[Rcpp::export]]
arma::mat compute_bd(arma::mat x, bool normalize) {
  arma::mat covx = arma::cov(x);
  arma::vec sqrt_diag_covx = arma::sqrt(covx.diag());
  arma::rowvec colmeans = arma::mean(x, 0);
  arma::mat xsweep = x.each_row() - colmeans;
  arma::mat bd = arma::abs(xsweep.each_row() / sqrt_diag_covx.t());
  if (normalize) {
    bd = bd * arma::diagmat(1 / arma::sum(x, 0));
  }
  return bd;
}


// [[Rcpp::export]]
arma::mat compute_md_and_bd(arma::mat x, bool normalize) {
  arma::mat covx = arma::cov(x);
  arma::vec sqrt_diag_covx = arma::sqrt(covx.diag());
  arma::rowvec colmeans = arma::mean(x, 0);
  arma::mat xsweep = x.each_row() - colmeans;
  arma::vec md = sum(xsweep * arma::pinv(arma::cov(x)) % xsweep, 1);
  arma::mat out = arma::abs(xsweep.each_row() / sqrt_diag_covx.t());
  if (normalize) {
    out = out * arma::diagmat(1 / arma::sum(x, 0));
  }
  out.insert_cols(0, md);
  return out;
}


// [[Rcpp::export]]
arma::mat compute_hc(int n, int p, int nsim) {
  arma::vec eig_vals(p);
  arma::mat eig_vecs(nsim, p);
  for(int i = 0; i < nsim; ++i) {
    if (i % 100 == 0) {
      Rcpp::checkUserInterrupt();  // check for user interuption
    }
    eig_vals = arma::eig_sym(arma::cov(arma::randn(n, p)));
    eig_vecs.row(i) = arma::sort(eig_vals, "descend").t();
  }
  return arma::mean(eig_vecs, 0).t();
}
