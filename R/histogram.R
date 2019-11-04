#' Irregular histograms
#'
#' The function \code{histogramr} computes an irregular histogram of the given
#' data values between 0 and 1. If \code{plot = TRUE}, the resulting object of
#' class \code{histogramr} is plotted by \code{plot.histogramr}, before
#' it is returned.
#'
#' The \code{method} option can be either of \code{"greedy"} or \code{"exact"}. The
#'    \code{"exact"} algorithm uses dynamic programming to find the true maximum
#'    likelihood estimator (if \code{type = "KL"}) or the true L2-minimzer
#'     (if \code{type = "L2"}). This method suffers from two drawbacks.
#'    It is considerable slower thant the
#'    greedy algorithm and tends to overfit. Choosing \code{method = "greedy"}
#'    is recommended. This runs a greedy coordinate search, which is much faster
#'    than the exact algorithm and is less prone to overfitting. When
#'    \code{greedy} is used, the options \code{init} and \code{modulator} can
#'    be passed to \code{control}. \code{init} specifies the inital break points
#'    in the coordinate search, while \code{modulator} limits the number of
#'    times the algorithms runs.
#'
#' If \code{weights} is \code{"uequal"} an ordinary irregular histogram is
#'    estimated, but if \code{weights} is \code{"equal"} an histogram with
#'    equal areas for every bin is estimated instead, see
#'    (Denby and Mallows, 2009). The option \code{"uequal"} is recommended.
#'
#' For maximum likelihood estimation choose \code{type} equal to \code{"KL"}.
#'    Choose \code{L2} for minimization of the L2 discrepancy. The \code{"L2"}
#'    option is slightly faster but \code{"KL"} gives slightly better results.
#'    The option \code{"KL"} is recommended.
#'
#' The \code{control} argument is a list that can supply any of the following
#'    components:
#'    \itemize{
#'     \item{\code{eps} The minimal distance between two break points. Defaults
#'       to \eqn{n^{-1/2}}}
#'     \item{\code{init} Only for \code{method = "greedy"}. Specifies the
#'       inital break points in the coordinate search.}
#'     \item{\code{modulator} Only for \code{method = "greedy"}. Limits the
#'        number of times the algorithm runs.}
#'    }
#'
#' @param x a vector of values for which the histogram is desired. \code{NA}
#'    values are automatically removed. \code{x} must be contained contained
#'    in \eqn{[0, 1]}.
#' @param breaks a single number giving the number of cells for the histogram.
#' @param method the method to be used. See ‘Details’. Can be abbreviated.
#' @param weights the weights to be used. See ‘Details’. Can be abbreviated.
#' @param type the type of discrepancy to minimize. See ‘Details’.
#' @param plot logical. If \code{TRUE} (default), a histogram is plotted,
#'     otherwise a list of breaks and counts is returned.
#' @param control a list of control parameters. See ‘Details’.
#' @param ... 	further arguments and graphical parameters passed to
#'     \code{plot.histogramr}.
#' @references
#' Denby, L., & Mallows, C. (2009). Variations on the histogram. Journal of
#' Computational and Graphical Statistics, 18(1), 21-31.
#' @export

histogramr <- function(x, breaks, method = c("greedy", "exact"),
                      weights = c("unequal", "equal"), type = c("KL", "L2"),
                      plot = TRUE, control, ...){

  x = x[!is.na(x)]

  if(length(x) < breaks - 1) stop("x must contain at least (breaks - 1) non-na values.")

  if(max(x) > 1 | min(x) < 0) stop("x must be contained in the unit interval")

  if(missing(breaks)) stop("Supply a number of break points, please.")

  x = sort(x)
  n = length(x)
  method = match.arg(method)
  weights = match.arg(weights)
  type = match.arg(type)

  if(missing(control)) control = NULL
  eps = if(is.null(control$eps)) eps = n^(-1/2) else control$eps

  if (method == "exact") {

    vals = cpp_exact(real_hist = (weights != "equal"),
                       l2 = (type == "L2"),
                       x = c(0, x, 1),
                       len = n,
                       k = breaks,
                       eps = eps)[breaks - 1, ]

  } else {

    modulator = if (is.null(control$modulator)) 10 else control$modulator
    init = if(is.null(control$init)) {
      stats::quantile(1:n, (1:(breaks - 1)) / breaks)
    } else control$init

    vals = cpp_greedy(real_hist = (weights != "equal"),
                      l2 = (type == "L2"),
                      x = c(0, x, 1),
                      len = n,
                      k = breaks,
                      modulator = modulator,
                      init = init,
                      eps = eps)[-breaks]

  }


  object = list()
  class(object) = c("histogramr")
  object$breaks = breaks
  object$density = calculate_weights(n, breaks, weights, vals)
  object$counts = object$density*n
  object$splits = x[vals]
  object$type = type
  object$weights = weights
  object$method = method
  object$control = control
  object$logLik = loglik(object, x)
  object$call = match.call()
  object$xname = deparse(match.call()[[2]])
  object$n = n
  object$support = c(0, 1)

  if(plot) plot(object, ...) else object

}
