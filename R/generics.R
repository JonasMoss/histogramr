#' @export
summary.histogramr = function(object, ...) {

  cat("\nIrregular histogram of", object$xname, "\n",
      "\nCall: ", deparse(object$call), "\n")
  cat("\nData:            ",  object$xname, " (", object$n, " obs.)\n",
      "Support:         (", object$support[1], ", ", object$support[2],   ")\n",
      "Breaks:          ", object$splits, "\n",
      "Break number:    ", object$n_breaks, "\n",
      "Density:         ", object$density, "\n",
      "Method:          ", object$method, "\n",
      "Weights:         ", object$weights, "\n",
      "Type:            ", object$type, "\n",
      sep = "")
  invisible(object)

}

#' @export
logLik.histogramr = function(object, ...) object$logLik