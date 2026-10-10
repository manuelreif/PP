#' Estimate Person Parameters
#' 
#' Compute person parameters for the 1,2,3,4-PL model and for the GPCM. Choose between ML, WL, MAP, EAP and robust estimation. Use this function if 4-PL items and GPCM items are mixed for each person.
#'
#' 
#' For a test with both: dichotomous and polytomous items which have been scaled under 1/2/3/4-PL model or the GPCM, use this function to estimate the person ability parameters. You have to define the appropriate model for each item. 
#' 
#'Please note, that  \code{robust} estimation with (Huber ability estimate) polytomous items is still experimental!
#' 
#'@param respm An integer matrix (or a data.frame), which contains the examinees responses. A persons x items matrix is expected. Missing responses are \code{NA}.
#'@param thres A numeric matrix which contains the threshold parameter for each item. If the first row of the matrix is not set to zero (only zeroes in the first row) - then a row-vector with zeroes is added by default.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected. A single value is used for all items. If \code{NULL} (default), all slopes are 1.
#'@param lowerA A numeric vector, which contains the lower asymptote parameters (kind of guessing parameter) for each item. In the case of polytomous items, the value must be 0. A single value is used for all items. If \code{NULL} (default), all lower asymptotes are 0.
#'@param upperA numeric vector, which contains the upper asymptote parameters for each item. In the case of polytomous items, the value must be 1. A single value is used for all items. If \code{NULL} (default), all upper asymptotes are 1.
#'@param theta_start A vector which contains a starting value for each person. If NULL is submitted, the starting values are set automatically. If a scalar is submitted, this start value is used for each person.
#'@param mu A numeric vector of location parameters for each person in case of MAP or EAP estimation. A single value is used for all persons. If nothing is submitted this is set to 0 for each person.
#'@param sigma2 A numeric vector of variance parameters for each person in case of MAP or EAP estimation. A single value is used for all persons. If nothing is submitted this is set to 1 for each person.
#'@param type Which maximization should be applied? There are five valid entries possible: "mle", "wle", "map", "eap" and "robust". To choose between the methods, or just to get a deeper understanding the papers mentioned below are quite helpful. The default is \code{"wle"} which is a good choice in many cases.
#'
#'@param model2est A character vector with length equal to the number of submitted items. It defines itemwise the response model under which the item parameter was estimated. There are 2 valid inputs up to now: \code{"GPCM"} and \code{"4PL"}. A \code{"4PL"} item must have exactly two categories in \code{thres}. If \code{NULL} (default), \code{\link{findmodel}(thres)} is used.
#'
#'@param maxsteps The maximum number of Newton steps per person. Default = 100.
#'@param exac How accurate are the estimates supposed to be? The iteration of a person stops when the step is smaller than \code{exac}. Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and \code{H} modulates how fast the downweighting takes place (for more Details read Schuster & Yuan 2011).
#'@param ctrl More controls:
#'
#'\itemize{
#' \item \code{killdupli}: Should duplicated response pattern be removed for estimation (estimation is faster)? This is especially resonable in case of a large number of examinees and a small number of items.  Use this option with caution (for map and eap), because persons with different \code{mu} and \code{sigma2} will have different ability estimates despite they responded identically. Default value is \code{FALSE}.
#'
#'\item \code{skipcheck}: Default = FALSE. If TRUE data matrix and arguments are not checked - this saves time e.g. when you use this function for simulations.
#'
#'}
#'@param range The interval \code{c(lower, upper)} in which mle, wle, map and robust estimates are searched. If the estimating equation has no root inside, the estimate is set to the bound it points to and its status is \code{"boundary"}. Default \code{c(-6, 6)}.
#'@param verbose If \code{TRUE}, a message names the model and the estimation type. Default \code{FALSE}.
#'
#'@template resulttemplate
#'
#' @seealso \link{PPass}, \link{PP_gpcm}, \link{PP_4pl}, \link{JKpp}, \link{PV}
#'
#'@export
#'
#'@author Manuel Reif
#'
#'@references Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory - Parameter Estimation Techniques. CRC-Press.
#'
#'Barton, M. A., & Lord, F. M. (1981). An Upper Asymptote for the Three-Parameter Logistic Item-Response Model.
#'
#'Magis, D. (2013). A note on the item information function of the four-parameter logistic model. Applied Psychological Measurement, 37(4), 304-315.
#'
#'Muraki, Eiji (1992). A Generalized Partial Credit Model: Application of an EM Algorithm. Applied Psychological Measurement, 16, 159-176.
#'
#'Muraki, Eiji (1993). Information Functions of the Generalized Partial Credit Model. Applied Psychological Measurement, 17, 351-363.
#'
#'Samejima, Fumiko (1993). The bias function of the maximum likelihood estimate of ability for the dichotomous response level. Psychometrika,  58, 195-209.
#'
#'Samejima, Fumiko (1993). An approximation of the bias function of the maximum likelihood estimate of a latent variable for the general case where the item responses are discrete. Psychometrika,  58, 119-138.
#'
#'Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability in item response models. Journal of Educational and Behavioral Statistics, 36(6), 720-735.
#'
#'Wang, S. and Wang, T. (2001). Precision of Warm's Weighted Likelihood Estimates for a Polytomous Model in Computerized Adaptive Testing. Applied Psychological Measurement, 25, 317-331.
#'
#'Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In Item Response Theory. Psychometrika, 54, 427-450.
#'
#'Yen, Y.-C., Ho, R.-G., Liao, W.-W., Chen, L.-J., & Kuo, C.-C. (2012). An empirical evaluation of the slip correction in the four parameter logistic models with computerized adaptive testing. Applied Psychological Measurement, 36, 75-87.
#'

#'@example ./R/.examples.R
#'@rdname PPall
#'
PPall <- function(respm, thres, slopes = NULL, lowerA = NULL, upperA = NULL, theta_start=NULL,
                  mu = NULL, sigma2 = NULL, type="wle", model2est = NULL,
                  maxsteps=100, exac=0.001, H=1, ctrl=list(), range = c(-6, 6), verbose = FALSE)
{
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  prep <- pp_prepare(respm, thres, slopes = slopes, lowerA = lowerA, upperA = upperA,
                     model2est = model2est, mu = mu, sigma2 = sigma2,
                     theta_start = theta_start, type = type, maxsteps = maxsteps,
                     exac = exac, H = H, ctrl = ctrl, range = range, verbose = verbose,
                     family = "mixed")
  pp_estimate(prep, call, "gpcm4pl",
              paste("mixed", paste(unique(prep$model2est), collapse = ", ")))
}
