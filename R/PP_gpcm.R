#' Estimate Person Parameters for the GPCM
#' 
#' Compute person parameters for the GPCM and choose between five common estimation techniques: ML, WL, MAP, EAP and a robust estimation. All item parameters are treated as fixed.
#'
#'
#' Please note, that \code{robust} estimation with (Huber ability estimate) polytomous items is still experimental!
#'
#'
#' The probability choosing the k-th category is as follows:
#'
#'\deqn{P(x_{ij} = k | \hat \alpha_i, \hat\beta_{iv}, \theta_j) = \frac{exp(\sum_{v=0}^{(k-1)}\hat \alpha_{i}(\theta_j - \hat \beta_{iv}))}{\,\sum_{c=0}^{m_i - 1}exp(\sum_{v=0}^{c}\hat \alpha_{i}(\theta_j - \hat \beta_{iv})))}}
#'
#'In our case \eqn{\theta} is to be estimated. The item parameters are assumed as fixed (usually these are estimates of a former scaling procedure).
#'
#'The model simplifies to the Partial Credit Model by setting \eqn{\alpha_{i} = 1, \forall i}.
#'
#' 
#'@param respm An integer matrix (or a data.frame), which contains the examinees responses. A persons x items matrix is expected. Missing responses are \code{NA}.
#'@param thres A numeric matrix which contains the threshold parameter for each item. If the first row of the matrix is not set to zero (only zeroes in the first row) - then a row-vector with zeroes is added by default.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected. A single value is used for all items. If \code{NULL} (default), all slopes are 1.
#'@param theta_start A vector which contains a starting value for each person. If NULL is submitted, the starting values are set automatically. If a scalar is submitted, this start value is used for each person.
#'@param mu A numeric vector of location parameters for each person in case of MAP or EAP estimation. A single value is used for all persons. If nothing is submitted this is set to 0 for each person.
#'@param sigma2 A numeric vector of variance parameters for each person in case of MAP or EAP estimation. A single value is used for all persons. If nothing is submitted this is set to 1 for each person.
#'@param type Which maximization should be applied? There are five valid entries possible: "mle", "wle", "map", "eap" and "robust". To choose between the methods, or just to get a deeper understanding the papers mentioned below are quite helpful. The default is \code{"wle"} which is a good choice in many cases.
#'@param maxsteps The maximum number of Newton steps per person. Default = 100.
#'@param exac How accurate are the estimates supposed to be? The iteration of a person stops when the step is smaller than \code{exac}. Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and \code{H} modulates how fast the downweighting takes place (for more Details read Schuster & Yuan 2011).
#'@param ctrl more controls
#'\itemize{
#' \item \code{killdupli}: Should duplicated response pattern be removed for estimation (estimation is faster)? This is especially resonable in case of a large number of examinees and a small number of items.  Use this option with caution (for map and eap), because persons with different \code{mu} and \code{sigma2} will have different ability estimates despite they responded identically. Default value is \code{FALSE}.
#'
#'\item \code{skipcheck}: Default = FALSE. If TRUE data matrix and arguments are not checked - this saves time e.g. when you use this function for simulations.
#'}
#'@param range The interval \code{c(lower, upper)} in which mle, wle, map and robust estimates are searched. If the estimating equation has no root inside, the estimate is set to the bound it points to and its status is \code{"boundary"}. Default \code{c(-6, 6)}.
#'@param verbose If \code{TRUE}, a message names the model and the estimation type. Default \code{FALSE}.
#'
#'@template resulttemplate
#'
#' @seealso \link{PPass}, \link{PPall}, \link{PP_4pl}, \link{JKpp}, \link{PV}
#'
#'@export
#'
#'@author Manuel Reif
#'@references Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory - Parameter Estimation Techniques. CRC-Press.
#'
#'Masters, G. N. (1982). A Rasch model for partial credit scoring. Psychometrika, 47(2), 149-174.
#'
#'Muraki, Eiji (1992). A Generalized Partial Credit Model: Application of an EM Algorithm. Applied Psychological Measurement, 16, 159-176.
#'
#'Muraki, Eiji (1993). Information Functions of the Generalized Partial Credit Model. Applied Psychological Measurement, 17, 351-363.
#'
#'Samejima, Fumiko (1993). The bias function of the maximum likelihood estimate of ability for the dichotomous response level. Psychometrika,  58, 195-209.
#'
#'Samejima, Fumiko (1993). An approximation of the bias function of the maximum likelihood estimate of a latent variable for the general case where the item responses are discrete. Psychometrika,  58, 119-138.
#'
#' Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability in item response models. Journal of Educational and Behavioral Statistics, 36(6), 720-735.
#'
#'Wang, S. and Wang, T. (2001). Precision of Warm's Weighted Likelihood Estimates for a Polytomous Model in Computerized Adaptive Testing. Applied Psychological Measurement, 25, 317-331.
#'
#'Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In Item Response Theory. Psychometrika, 54, 427-450.
#'

#'@example ./R/.examples_gpcm.R
#'@keywords Person Parameters GPCM
#'@rdname PP_gpcm
#'




PP_gpcm <- function(respm, thres, slopes = NULL, theta_start=NULL,
                    mu = NULL, sigma2 = NULL, type="wle", maxsteps=100, exac=0.001, H=1, ctrl=list(),
                    range = c(-6, 6), verbose = FALSE)
{
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  prep <- pp_prepare(respm, thres, slopes = slopes, mu = mu, sigma2 = sigma2,
                     theta_start = theta_start, type = type, maxsteps = maxsteps,
                     exac = exac, H = H, ctrl = ctrl, range = range, verbose = verbose,
                     family = "gpcm")
  pp_estimate(prep, call, "gpcm", "GPCM")
}
