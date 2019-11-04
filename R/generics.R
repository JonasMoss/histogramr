#' Wrangles arguments for use in the plot, lines and points functions.
#'
#' @param object A \code{histogramr} object.
#' @param ... Passed to \code{plot} down the line.
#' @keywords internal
plot_wrangler = function(x, ...) {

  defaults = list(xlab = x$xname,
                  ylab = "Density",
                  bty = "n",
                  main = paste("Histogram of", x$xname),
                  lwd  = 1)

  args = listmerge(x = defaults,
                   y = list(...))

  splits = c(0, x$splits, 1)
  diffs = sapply(1:x$breaks, function(i) 1 / (splits[i + 1] - splits[i]))
  args$x = splits
  args$y = c(0, x$density * diffs)
  args

}

#' Plot and Lines Methods for Irregular Histograms
#'
#' The \code{plot} and \code{lines} methods for \code{histogramr} objects.
#'
#' @export
#' @param x a \code{histogramr} object.
#' @param ... parameters passed to \code{plot}, \code{lines}, or \code{points}.
#' @return An invisible copy of \code{x}.
#' @examples
#'   histogramr(USArrests$Rape/100, breaks = 7)
#' @export
plot.histogramr = function(x, ...) {

  args = plot_wrangler(x, ...)
  args$type = type = "S"
  do.call(graphics::plot, args)
  args$type = type = "h"
  do.call(graphics::lines, args)
  invisible(x)

}

#' @export
#' @rdname plot.histogramr
lines.histogramr = function(x, ...) {

  args = plot_wrangler(x, ...)
  args$type = type = "S"
  do.call(graphics::lines, args)
  args$type = type = "h"
  do.call(graphics::lines, args)
  invisible(x)

}

#' @export
summary.histogramr = function(object, ...) {

  cat("\nIrregular histogram of", object$xname, "\n",
      "\nCall: ", deparse(object$call), "\n")
  cat("\nData:            ",  object$xname, " (", object$n, " obs.)\n",
      "Support:         (", object$support[1], ", ", object$support[2],   ")\n",
      "Breaks:          ", object$breaks, "\n",
      "Break points:    ", object$splits, "\n",
      "Density:         ", object$density, "\n",
      "Method:          ", object$method, "\n",
      "Weights:         ", object$weights, "\n",
      "Type:            ", object$type, "\n",
      sep = "")
  invisible(object)

}

#' @export
logLik.histogramr = function(object, ...) object$logLik