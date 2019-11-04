#include "shared.h"

// [[Rcpp::export]]
Rcpp::NumericVector cpp_greedy(bool real_hist,
                               bool l2,
                               Rcpp::NumericVector x,
                               int len,
                               double k,
                               int modulator,
                               Rcpp::NumericVector init,
                               double eps){

    double (*residual)(int, int, double*, int, double, double);
    double (*residual_final)(int, double*, int, double, double);

    /* residual and residual_final point to the functions needed in maximization.
     * The definition of these functions vary as L2 and real varies, every other aspect
     * of the algorithm stays constant.
     */

    if (real_hist) {
        if (l2) {
            residual = &rlt;
            residual_final = &rlt_final;
        }
        else {
            residual = &rkt;
            residual_final = &rkt_final;
        }
    }

    else {
        if (l2) {
            residual = &rlf;
            residual_final = &rlf_final;

        }
        else {
            residual = &rkf;
            residual_final = &rkf_final;
        }
    }

    /* We define the pretty "matrix" estimatse indices, with the correct dimensions.
     * This matrix contains the ML / L2 estimate indices, as j-ary vectors. The (i,j)-th
     * element corresponds to ML-estimate with k=j and x[0:j]. */

    std::vector <int> estimates;
    std::vector <int> test_estimates;

    estimates.resize((int) k+1);
    test_estimates.resize((int) k+1);
    estimates[0] = 0;
    estimates[k] = 1;

    for(int i = 1; i < k; i++) {
      estimates[i] = init[i - 1];
    }

    int ended = 0;
    int over;
    int under;
    double max;
    double temp;

    for (int j = 0; j < modulator * k; j++) {

      test_estimates = estimates;

      /* The loop takes care of all the values except the final. */

      for (int i = 1; i < (k - 1); i++) {

        over = estimates[i + 1];
        under = estimates[i - 1];
        max = -0.1/0.0;

        for (int p = under; p<over; p++){

          if (x[p]-x[under]>eps && x[over] - x[p]> eps) {

            temp = residual(under, p, x. begin(), len, k, eps) + residual(p, over, x.begin(), len, k, eps);

          } else temp = -0.1/0.0;

          if (max < temp) {

            max = temp;
            estimates[i] = p;

          }

        }

      }

      /* And now is the time for the last value. */
      int i = (k - 1);
      under = estimates[i - 1];
      max = -0.1/0.0;

      for (int p = under; p < (len + 1); p++){

        if (x[p] - x[under] > eps && 1 - x[p] > eps){
            temp = residual(under, p, x.begin(), len, k, eps) + residual_final(p, x.begin(), len, k, eps);
          }

        else temp = -0.1/0.0;

        if (max < temp) {
            max = temp;
            estimates[i] = p;
          }
      }

      /* We test the break condition. */

      if (test_estimates == estimates) {
        ended = j;
        break;
      }

    }

    Rcpp::NumericVector xx((int) k);

    for (int i=0; i < k - 1; i++){
        xx[i] = estimates[i + 1];
      }

    xx[k - 1] = ended;

    return(xx);

}
