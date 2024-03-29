#include "shared.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_exact(bool real_hist,
                              bool l2,
                              Rcpp::NumericVector x,
                              int len,
                              double k,
                              double eps){

    /* residual and residual_final point to the functions needed in maximization.
     * The definition of these functions vary as L2 and real varies, every other aspect
     * of the algorithm stays constant.
     */

    double (*residual)(int, int, double*, int, double, double);
    double (*residual_final)(int, double*, int, double, double);

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

    /* We define the pretty "matrix" of estimates indices, with the correct dimensions.
     * This matrix contains the ML / L2 estimate indices, as j-ary vectors. The (i,j)-th
     * element corresponds to ML-estimate with k=j and x[0:j]. */

    std::vector < std::vector < std::vector < int > > > estimates;
    estimates.resize(len+2);

    for (int i = 0; i <= len+1; i++){

        estimates[i].resize(k);

        for (int j = 0;j<(k-1);j++){

            estimates[i][j].resize(j+1);

        }

    }

    /* This matrix contains the optimal objective values at (i,j) instead of the estimates.
     * It doesn't need crazy dimensions. */

    std::vector < std::vector < double > > objective;
    objective.resize(len+2);

    for (int i = 0; i <= len + 1; i++) {

        objective[i].resize(k - 1);

    }

    /* We begin on the actual algorithm. */


    /* The special case when j = 0. Needed in order to get initial values. Also, the case when i = len+1 is
     * extra special. However, it's only needed when k = 2. */

    std::vector <int> est;
    est.resize(1);

    for (int i = 2; i <= len; i++){

        double maxer = residual(0, 1, x.begin(), len, k, eps) + residual(1, i, x.begin(), len, k, eps);
        double temp_max;
        int ind = 1;

        for (int p=2; p<i; p++){

            temp_max = residual(0, p, x.begin(), len, k, eps) + residual(p, i, x.begin(), len, k, eps);

            if (temp_max > maxer){

                maxer = temp_max;
                ind = p;

            }
        }

        est[0] = ind;
        estimates[i][0] = est;
        objective[i][0] = maxer;
    }

    /* Now we can handle the case when j=0 and i = len + 1! */

    int i = len + 1;
    double maxer = residual(0, 1, x.begin(), len, k, eps) + residual_final(1, x.begin(), len, k, eps);
    double temp_max;
    int ind = 1;

    for (int l=2; l<i; l++){
        temp_max = residual(0, l, x.begin(), len, k, eps) + residual_final(l, x.begin(), len, k, eps);

        if (temp_max > maxer) {

            maxer = temp_max;
            ind = l;

        }

    }

    est[0] = ind;
    estimates[i][0] = est;
    objective[i][0] = maxer;



    /* The main program follows, the generation of the two matrices objective and estimates.
     * We begin with iteration through j, as the calculation of estimates[i,j] depends on knowing (almost) every
     * value estimates[i',j], with i' < i. */

    for (int j = 1; j < (k-1); j++) {

        // Initialization of variables used in loop.
        std::vector <int> est;
        est.resize(j+1);
        double maxer, temp_max;
        int ind;


        /* Calculates the matrix for every term except i=len+1, which is a special case. */
        for (int i = j + 2; i < len + 1; i++) {

            /* Given an i, we wish to find the best estimates for x[0,i]
             * given that k=j. We use i = j+2 in order to have enough points to fit the x:
             * The "best" case is that (i-1) is the optimal index, and this one needs j points
             * of x below it. We start with i-1, and continue trough the loop. */

            maxer = objective[i - 1][j - 1] + residual(i - 1, i, x.begin(), len, k, eps);
            ind = i - 1;

            /* We have the condition p>=j+1 for the same reason as above. If p = j or less,
             * there won't be the needed j points below it. */

            for (int p = (i - 2); p >= (j+1); p--){
                temp_max = objective[p][j - 1] + residual(p, i, x.begin(), len, k, eps);
                if (temp_max > maxer){
                    maxer = temp_max;
                    ind = p;
                }
            }

            /* Our resulting objective is maxer, while our indices are, the winning estimates'
             * indices concatenated with with the index which makes them win. */

            objective[i][j] = maxer;
            est = estimates[ind][j - 1];
            est.push_back(ind);
            estimates[i][j] = est;
        }

        /* We proceed with the special case i = len+1. The reason why this is a special case is
         * that Pn(1) = Pn(x_n), the final observation in the x set. This makes the residual function
         * return incorrect values.
        */

        int i = len + 1;
        maxer = objective[i - 1][j - 1];
        ind = i-1;

        for (int p = (i - 2); p >= (j + 1); p--) {

            temp_max = objective[p][j - 1] + residual_final(p, x.begin(), len, k, eps);

            if (temp_max > maxer){

                maxer = temp_max;
                ind = p;

            }
        }

        objective[i][j] = maxer;
        est = estimates[ind][j - 1];
        est.push_back(ind);
        estimates[i][j] = est;

        /* We print out the content of the vector. */

    }

    Rcpp::NumericMatrix xx = Rcpp::NumericMatrix(Rcpp::Dimension(k-1, k-1));

    for (int i = 0; i < k - 1; i++) {

      for (int j = 0; j <= i; j++) {

        xx(i,j) = estimates[len + 1][i][j];

      }

    }

    return(xx);

}
