#include "shared.h"

//* These correspond to Kullback-Leibler and equal weights. */
double rkf(int i, int j, double* x, int len, double k, double eps) {
  double a = -(j - i);
  double b = x[j] - x[i];
  if (b < eps) return -0.1 / 0.0;
  else return(a * std::log(b));
}

double rkf_final(int i,double* x,int len, double k, double eps) {
  double a = -(len-i);
  double b = 1 - x[i];
  if (b < eps) return -0.1/0.0;
  else return(a*std::log(b));
}

/* These correspond to L2 and equal weights. */
double rlf(int i, int j, double* x, int len, double k, double eps) {
  double a = 2*(j - i)/((double) len) - 1/k;
  double b = x[j] - x[i];
  if (b < eps) return -0.1/0.0;
  else return(a/b);
}

double rlf_final(int i, double* x, int len, double k, double eps) {
  double a = 2 * (len-i)/((double) len) - 1/k;
  double b = 1 - x[i];
  if (b < eps) return -0.1/0.0;
  else return(a/b);
}

/* ... and these correspond to KL weights and splits! */
double rkt(int i,int j,double* x,int len,double k,double eps) {
  double b = x[j]-x[i];
  if (b < eps) return -0.1/0.0;
  else return((j-i)*(std::log(j-i)-std::log(len)-std::log(b)));
}

double rkt_final(int i, double* x, int len, double k, double eps){
  double b = 1-x[i];
  if (b < eps) return -0.1/0.0;
  else return((len-i)*(std::log(len-i)-std::log(len)-std::log(b)));
}

/* Finally, L2 weights and splits. */
double rlt(int i, int j, double* x, int len, double k, double eps){
  double a = pow(j-i,2);
  double b = x[j]-x[i];
  if (b < eps) return -0.1/0.0;
  else return(a/b);
}

double rlt_final(int i, double* x, int len, double k, double eps){
  double a = pow(1-i,2);
  double b = 1-x[i];
  if (b < eps) return -0.1/0.0;
  else return(a/b);
}
