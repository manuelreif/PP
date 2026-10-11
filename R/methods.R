#' Methods for estimation results
#'
#' Print, summarise and extract the person estimates of \code{PP_4pl()}, \code{PP_gpcm()} and \code{PPall()} (objects of class \code{ppeo}).
#'
#'@param x,object An object of class \code{ppeo}.
#'@param digits Number of significant digits to print.
#'@param nrowmax When printing the estimates - how many rows should be shown? Default = 15.
#'@param row.names,optional Ignored; the rows are the persons in the order of the response matrix.
#'@param ... Further arguments, passed to \code{print()}.
#'
#'@return \code{coef()} returns the estimates as a vector (persons in the order of the response matrix), \code{as.data.frame()} the data.frame \code{$estimates} (estimate, SE, nsteps, status). \code{print()} and \code{summary()} return the object invisibly.
#'
#' @seealso \link{PP_4pl}, \link{PP_gpcm}, \link{PPall}
#'@name ppeo-methods
NULL

#'@rdname ppeo-methods
#'@export
print.ppeo <- function(x, digits = 4, ...) {
  print(x$estimates, digits = digits, ...)
  invisible(x)
}

#'@rdname ppeo-methods
#'@export
summary.ppeo <- function(object, nrowmax = 15, digits = 4, ...) {
  cat("PP Version: ", as.character(attr(object$call, "version")), "\n")
  cat("\n Call:", deparse(object$call), "\n- job started @", attr(object$call, "date"), "\n\n")
  cat("Estimation type:", object$type, "\n\n")
  est <- object$estimates
  cat("Status of the persons:\n")
  print(table(est$status))
  if (object$type != "eap") cat("\nNewton steps: at most", max(0L, est$nsteps), "\n")
  cat("-------------------------------------\n")
  print(utils::head(est, nrowmax), digits = digits, ...)
  if (nrow(est) > nrowmax) cat("--------> output truncated <--------\n")
  invisible(object)
}

#'@rdname ppeo-methods
#'@export
coef.ppeo <- function(object, ...) {
  object$estimates$estimate
}

#'@rdname ppeo-methods
#'@export
as.data.frame.ppeo <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$estimates
}
