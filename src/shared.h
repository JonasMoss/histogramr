#ifndef SHARED
#define SHARED

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

const double MIN_DIFF_EPS = 1e-8;

bool tol_equal(const double& x, const double& y);

double rkf(int i,int j,double* data,int len,double k,double lim);
double rkf_final(int i,double* data,int len,double k,double lim);
double rlf(int i,int j,double* data,int len,double k,double lim);
double rlf_final(int i,double* data,int len,double k,double lim);
double rkt(int i,int j,double* data,int len,double k,double lim);
double rkt_final(int i,double* data,int len,double k,double lim);
double rlt(int i,int j,double* data,int len,double k,double lim);
double rlt_final(int i,double* data,int len,double k,double lim);


#endif
