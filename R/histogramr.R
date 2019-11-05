#' Irregular histograms
#'
#' The function \code{histogramr} computes an irregular histogram of the given
#' data values between 0 and 1. If \code{plot = TRUE}, the resulting object of
#' class \code{histogramr} is plotted by \code{plot.histogramr}, before
#' it is returned.
#'
#' The \code{method} option can be either of \code{"greedy"} or \code{"exact"}.
#'    The \code{"exact"} algorithm uses dynamic programming to find the true
#'    maximum likelihood estimator (if \code{type = "KL"}) or the true
#'    L2-minimzer (if \code{type = "L2"}). This method suffers from two
#'    drawbacks. It is considerable slower thant the
#'    greedy algorithm and tends to overfit. Choosing \code{method = "greedy"}
#'    is recommended. This runs a greedy coordinate search, which is much faster
#'    than the exact algorithm and is less prone to overfitting. When
#'    \code{greedy} is used, the options \code{init} and \code{modulator} can
#'    be passed to \code{control}. \code{init} specifies the inital break points
#'    in the coordinate search, while \code{modulator} limits the number of
#'    times the algorithms runs.
#'
#' If \code{constraint} is \code{"none"} an ordinary irregular histogram is
#'    estimated, but if \code{constraint} is \code{"equal_area"} an histogram
#'    with equal areas for every bin is estimated instead, see
#'    (Denby and Mallows, 2009). An equal-area histogram is not a bona fide
#'    histogram in the sense that the area of a bin can fail to be proportial
#'    to the number of observations falling in it. The option \code{"uequal"}
#'    is recommended.
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
#' @param constraint the constraint to use on the bins. See ‘Details’.
#'     Can be abbreviated.
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
                      constraint = c("none", "equal_area"),
                      type = c("KL", "L2"), plot = TRUE, control, ...) {

  x = x[!is.na(x)]

  if (missing(breaks))
    stop("Supply a number of break points, please.")

  if (length(x) < breaks - 1)
    stop("x must contain at least (breaks - 1) non-na values.")

  if (max(x) > 1 | min(x) < 0)
    stop("x must be contained in the unit interval")

  x = sort(x)
  n = length(x)
  method = match.arg(method)
  constraint = match.arg(constraint)
  type = match.arg(type)
  support = c(0, 1)

  if (missing(control)) control = NULL

  if (is.null(control$eps)) control$eps = n ^ (- 1 / 2)

  if (method == "exact") {

    vals = cpp_exact(real_hist = (constraint != "equal_area"),
                       l2 = (type == "L2"),
                       x = c(0, x, 1),
                       len = n,
                       k = breaks,
                       eps = control$eps)[breaks - 1, ]

  } else {

    if (is.null(control$modulator)) control$modulator = 10

    if (is.null(control$init)) {

      control$init = stats::quantile(1:n, (1:(breaks - 1)) / breaks)

    }

    vals = cpp_greedy(real_hist = (constraint != "equal_area"),
                      l2 = (type == "L2"),
                      x = c(0, x, 1),
                      len = n,
                      k = breaks,
                      modulator = control$modulator,
                      init = control$init,
                      eps = control$eps)[-breaks]

  }

  area = area(n, breaks, constraint, vals)

  object = list()
  class(object) = c("histogram", "histogramr")

  object$breaks = c(support[1], x[vals], support[2])
  object$density = area * 1 / diff(object$breaks)
  object$mids = diff(object$breaks) / 2 + utils::head(object$breaks, -1)
  object$counts = area * n
  object$xname = deparse(match.call()[[2]])
  object$equidist = FALSE

  attr(object, "type") = type
  attr(object, "constraint") = constraint
  attr(object, "area") = area
  attr(object, "method") = method
  attr(object, "control") = control
  attr(object, "logLik") = loglik(object, x)
  attr(object, "n") = n
  attr(object, "support") = support
  attr(object, "call") = match.call()

  if (plot) plot(object, freq = FALSE, ...) else object

}
