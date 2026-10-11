#'@return An object of class \code{ppeo} (and \code{fourpl}, \code{gpcm} or \code{gpcm4pl}), a list with
#'\describe{
#'\item{\code{estimates}}{A data.frame with one row per person (row names from \code{respm}): \code{estimate}, \code{SE}, \code{nsteps} (Newton steps of the person; 0 for eap) and \code{status}, a factor with the levels
#'\itemize{
#'\item \code{"ok"}: converged inside \code{range},
#'\item \code{"extreme"}: mle or robust estimate of a person with all answers in the lowest or highest category, the estimate is \code{-Inf} or \code{Inf} and the SE \code{NA},
#'\item \code{"boundary"}: the estimating equation has no root inside \code{range}, the estimate is the bound,
#'\item \code{"no_response"}: the person answered no item, estimate and SE are \code{NA} (eap: prior mean and SD),
#'\item \code{"not_converged"}: no convergence within \code{maxsteps}.
#'}
#'A warning reports persons with status \code{"boundary"} or \code{"not_converged"}.}
#'\item{\code{type}, \code{call}}{The estimation type and the call.}
#'\item{\code{model}}{The checked and prepared inputs.}
#'\item{\code{resPP}}{The result in the layout of earlier versions: \code{resPP$resPP} is the persons x 2 matrix of estimates and SEs, \code{resPP$nsteps} the largest number of steps.}
#'\item{\code{ipar}}{The inputs in the layout of earlier versions; used internally by \code{Pfit}.}
#'}
#'Use \code{coef()} for the estimates and \code{as.data.frame()} for the table.
