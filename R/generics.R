#' @export
summary.histogramr = function(object, ...) {

  digits = list(...)$digits
  if(is.null(digits)) digits = 3

  cat("\nIrregular histogram of", object$xname, "\n",
      "\nCall: ", deparse(attr(object, "call")), "\n")

  cat("\nBreaks:\n  ")
  print.default(format(object$breaks, digits = digits), print.gap = 2L, quote = FALSE)
  cat("\nDensity:\n  ")
  print.default(format(object$density, digits = digits), print.gap = 2L, quote = FALSE)

  cat("\nData:            ",  object$xname, " (", attr(object, "n"), " obs.)\n",
      "Support:         (", attr(object, "support")[1], ", ",
                            attr(object, "support")[2],   ")\n",
      "Break number:    ", length(object$breaks), "\n",
      "Method:          ", attr(object, "method"), "\n",
      "Constraint:      ", attr(object, "constraint"), "\n",
      "Type:            ", attr(object, "type"), "\n\n",
      sep = "")

  invisible(object)

}

#' @export
logLik.histogramr = function(object, ...) attr(object, "logLik")