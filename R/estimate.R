# Estimation layer: the wrappers PP_4pl(), PP_gpcm() and PPall() prepare the
# inputs with pp_prepare(); pp_estimate() runs the estimator and returns the
# result object built by new_ppeo(). No rows are removed or re-inserted:
# persons without answers and extreme scores are handled per person and marked
# in the status.

#' Estimate Person Parameters for the 4-PL model
#' 
#' Compute Person Parameters for the 1/2/3/4-PL model and choose between five common estimation techniques: ML, WL, MAP, EAP and a robust estimation. All item parameters are treated as fixed.
#'
#' 
#' With this function you can estimate:
#' \itemize{
#' \item \bold{1-PL model} (Rasch model) by submitting: the data matrix, item difficulties and \bold{nothing else}, since the 1-PL model is merely a 4-PL model with: any slope = 1, any lower asymptote = 0 and any upper asymptote = 1!
#' \item \bold{2-PL model} by submitting: the data matrix, item difficulties and slope parameters. Lower and upper asymptotes are automatically set to 0 and 1 respectively.
#' \item \bold{3-PL model} by submitting anything except the upper asymptote parameters
#' \item \bold{4-PL model} by submitting all parameters.
#'}
#' 
#' The probability function of the 4-PL model is:
#'\deqn{P(x_{ij} = 1 | \hat \alpha_i, \hat\beta_i, \hat\gamma_i, \hat\delta_i, \theta_j ) = \hat\gamma_i + (\hat\delta_i-\hat\gamma_i) \frac{exp(\hat \alpha_i (\theta_{j} - \hat\beta_{i}))}{\,1 + exp(\hat\alpha_i (\theta_{j} - \hat\beta_{i}))}}
#' 
#' In our case \eqn{\theta} is to be estimated, and the four item parameters are assumed as fixed (usually these are estimates of a former scaling procedure).
#' 
#' The 3-PL model is the same, except that \eqn{\delta_i = 1, \forall i}.
#' 
#' In the 2-PL model \eqn{\delta_i = 1, \gamma_i = 0, \forall i}.
#' 
#' In the 1-PL model \eqn{\delta_i = 1, \gamma_i = 0, \alpha_i = 1, \forall i}.
#' 
#' The \bold{robust} estimation method, applies a Huber-type estimator (Schuster & Yuan, 2011), which downweights responses to items which provide little information for the ability estimation. First a residuum is estimated and on this basis, the weight for each observation is computed.
#' 
#' residuum:
#' \deqn{r_i = \alpha_i(\theta - \beta_i)}
#' 
#' weight:
#' 
#' \deqn{w(r_i) =  1 \rightarrow if\, |r_i| \leq H}
#' \deqn{w(r_i) = H/|r| \rightarrow if\, |r_i| > H}
#' 
#'@param respm An integer matrix (or a data.frame), which contains the responses of the examinees. A persons x items matrix is expected. Missing responses are \code{NA}.
#'@param thres A numeric vector or a numeric matrix which contains the threshold parameter (also known as difficulty parameter or beta parameter) for each item. A matrix with one column per item is also accepted; its first row of zeros (as in \code{\link{PP_gpcm}}) is optional.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected. A single value is used for all items. If \code{NULL} (default), all slopes are 1.
#'@param lowerA A numeric vector, which contains the lower asymptote parameters (kind of guessing parameter) for each item. A single value is used for all items. If \code{NULL} (default), all lower asymptotes are 0.
#'@param upperA numeric vector, which contains the upper asymptote parameters for each item. A single value is used for all items. If \code{NULL} (default), all upper asymptotes are 1.
#'@param theta_start A vector which contains a starting value for each person. If NULL is submitted, the starting values are set automatically. If a scalar is submitted, this start value is used for each person.
#'@param mu A numeric vector of location parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 0 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param sigma2 A numeric vector of variance parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 1 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param type Which maximization should be applied? There are five valid entries possible: "mle", "wle", "map", "eap" and "robust". To choose between the methods, or just to get a deeper understanding the papers mentioned below are quite helpful. The default is \code{"wle"} which is a good choice in many cases. With \code{"eap"} the estimate is the mean and the SE the standard deviation of the posterior, computed on a grid of ability values (see \link{PV}). Plausible values (\code{\link{PV}}) need the prior of \code{"eap"} or \code{"map"}.
#'@param maxsteps The maximum number of Newton steps per person. Default = 100.
#'@param exac How accurate are the estimates supposed to be? The iteration of a person stops when the step is smaller than \code{exac}. Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and \code{H} modulates how fast the downweighting takes place (for more Details read Schuster & Yuan 2011). 
#'@param ctrl No longer used. Its options \code{killdupli} and \code{skipcheck} of version 0.6.4 saved time with the old estimation routine; since 0.7.0 every person is estimated separately, and options given are ignored with a warning.
#'@param range The interval \code{c(lower, upper)} in which mle, wle, map and robust estimates are searched. If the estimating equation has no root inside, the estimate is set to the bound it points to and its status is \code{"boundary"}. Default \code{c(-6, 6)}.
#'@param verbose If \code{TRUE}, a message names the model and the estimation type. Default \code{FALSE}.
#'
#'@template resulttemplate
#'
#' @seealso \link{PPass}, \link{PPall}, \link{PP_gpcm}, \link{JKpp}, \link{PV}
#'
#'@export
#'
#'@author Manuel Reif
#'@references 
#'Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory - Parameter Estimation Techniques. CRC-Press.
#'
#' Barton, M. A., & Lord, F. M. (1981). An Upper Asymptote for the Three-Parameter Logistic Item-Response Model.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring an examinee's ability. In Lord, F.M. & Novick, M.R. (Eds.), Statistical theories of mental test scores. Reading, MA: Addison-Wesley.
#'
#'Magis, D. (2013). A note on the item information function of the four-parameter logistic model. Applied Psychological Measurement, 37(4), 304-315.
#'
#'Samejima, Fumiko (1993). The bias function of the maximum likelihood estimate of ability for the dichotomous response level. Psychometrika,  58, 195-209.
#'
#'Samejima, Fumiko (1993). An approximation of the bias function of the maximum likelihood estimate of a latent variable for the general case where the item responses are discrete. Psychometrika,  58, 119-138.
#'
#'Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability in item response models. Journal of Educational and Behavioral Statistics, 36(6), 720-735.
#'
#'Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In Item Response Theory. Psychometrika, 54, 427-450.
#'
#'Yen, Y.-C., Ho, R.-G., Liao, W.-W., Chen, L.-J., & Kuo, C.-C. (2012). An empirical evaluation of the slip correction in the four parameter logistic models with computerized adaptive testing. Applied Psychological Measurement, 36, 75-87.
#'
#'@examples
#' ## real data: an adaptive intelligence test with known item difficulties (1PL)
#' data(pp_amt)
#' d <- as.matrix(pp_amt$daten_amt[, -(1:7)])
#' res <- PP_4pl(respm = d, thres = pp_amt$betas[, 2], type = "wle")
#' summary(res)
#'
#' ## simulated data: 2PL, 3PL and 4PL
#' set.seed(1522)
#' b  <- seq(-3, 3, length = 12)
#' sl <- round(runif(12, 0.5, 1.5), 2)
#' la <- round(runif(12, 0, 0.25), 2)
#' ua <- round(runif(12, 0.8, 1), 2)
#' x  <- sim_4pl(beta = b, alpha = sl, lowerA = la, upperA = ua, theta = rnorm(50))
#'
#' res2pl <- PP_4pl(respm = x, thres = b, slopes = sl, type = "mle")
#' res3pl <- PP_4pl(respm = x, thres = b, slopes = sl, lowerA = la, type = "wle")
#' res4pl <- PP_4pl(respm = x, thres = b, slopes = sl, lowerA = la, upperA = ua, type = "map")
#'
#' # persons with all answers right (or wrong) have no finite ML estimate
#' table(res2pl$estimates$status)
#' head(res3pl$estimates)
#' coef(res4pl)[1:5]
#'
#' ## EAP with person-specific priors
#' res_eap <- PP_4pl(respm = x, thres = b, slopes = sl, type = "eap",
#'                   mu = rep(c(-0.5, 0.5), 25), sigma2 = 0.8)
#'
#' ## robust estimation: the example of Schuster & Yuan (2011)
#' AWM <- matrix(0, 7, 7)
#' diag(AWM) <- 1
#' coef(PP_4pl(respm = AWM, thres = -3:3, type = "mle"))
#' coef(PP_4pl(respm = AWM, thres = -3:3, type = "robust"))
#'@keywords Person Parameters 4pl
#'@rdname PP_4pl
#'
PP_4pl <- function(respm, thres, slopes=NULL, lowerA=NULL, upperA=NULL, theta_start=NULL,
                   mu = NULL, sigma2 = NULL, type="wle", maxsteps=100, exac=0.001, H=1, ctrl=list(),
                   range = c(-6, 6), verbose = FALSE)
{
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  # model name for the message only; pp_prepare() fills in the missing parameters
  modest <- if (is.null(lowerA) && is.null(upperA) && is.null(slopes)) "1pl"
            else if (is.null(lowerA) && is.null(upperA)) "2pl"
            else if (is.null(upperA)) "3pl"
            else if (is.null(lowerA)) "3pl_upperA"
            else "4pl"

  prep <- pp_prepare(respm, thres, slopes = slopes, lowerA = lowerA, upperA = upperA,
                     mu = mu, sigma2 = sigma2, theta_start = theta_start, type = type,
                     maxsteps = maxsteps, exac = exac, H = H, ctrl = ctrl, range = range,
                     verbose = verbose, family = "4pl")
  pp_estimate(prep, call, "fourpl", paste(modest, "model"))
}

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
#'@param respm An integer matrix (or a data.frame), which contains the responses of the examinees. A persons x items matrix is expected. Missing responses are \code{NA}.
#'@param thres A numeric matrix which contains the threshold parameter for each item. If the first row of the matrix is not set to zero (only zeroes in the first row) - then a row-vector with zeroes is added by default.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected. A single value is used for all items. If \code{NULL} (default), all slopes are 1.
#'@param theta_start A vector which contains a starting value for each person. If NULL is submitted, the starting values are set automatically. If a scalar is submitted, this start value is used for each person.
#'@param mu A numeric vector of location parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 0 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param sigma2 A numeric vector of variance parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 1 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param type Which maximization should be applied? There are five valid entries possible: "mle", "wle", "map", "eap" and "robust". To choose between the methods, or just to get a deeper understanding the papers mentioned below are quite helpful. The default is \code{"wle"} which is a good choice in many cases. With \code{"eap"} the estimate is the mean and the SE the standard deviation of the posterior, computed on a grid of ability values (see \link{PV}). Plausible values (\code{\link{PV}}) need the prior of \code{"eap"} or \code{"map"}.
#'@param maxsteps The maximum number of Newton steps per person. Default = 100.
#'@param exac How accurate are the estimates supposed to be? The iteration of a person stops when the step is smaller than \code{exac}. Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and \code{H} modulates how fast the downweighting takes place (for more Details read Schuster & Yuan 2011).
#'@param ctrl No longer used. Its options \code{killdupli} and \code{skipcheck} of version 0.6.4 saved time with the old estimation routine; since 0.7.0 every person is estimated separately, and options given are ignored with a warning.
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
#'@examples
#' # six items with three categories: one column of thresholds per item
#' thres <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
#' sl <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
#' set.seed(1750)
#' x <- sim_gpcm(thres = thres, alpha = sl, theta = rnorm(30))
#'
#' # GPCM, weighted likelihood
#' resgpcm <- PP_gpcm(respm = x, thres = thres, slopes = sl, type = "wle")
#' summary(resgpcm)
#'
#' # partial credit model: all slopes 1 (the default), EAP
#' respcm <- PP_gpcm(respm = x, thres = thres, type = "eap")
#' head(respcm$estimates)
#'
#' # items with a different number of categories: NA below the last threshold
#' thres4 <- rbind(thres, c(NA, NA, NA, NA, 1.7, 1))
#' x4 <- sim_gpcm(thres = thres4, alpha = sl, theta = rnorm(30))
#' head(PP_gpcm(respm = x4, thres = thres4, slopes = sl, type = "map")$estimates)
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

#' Estimate Person Parameters
#' 
#' Compute person parameters for the 1,2,3,4-PL model and for the GPCM. Choose between ML, WL, MAP, EAP and robust estimation. Use this function if 4-PL items and GPCM items are mixed for each person.
#'
#' 
#' For a test with both: dichotomous and polytomous items which have been scaled under 1/2/3/4-PL model or the GPCM, use this function to estimate the person ability parameters. The model of each item follows from \code{thres}: an item with one threshold is a 4PL item (with \code{slopes}, \code{lowerA} and \code{upperA}), an item with several thresholds a GPCM item (see \code{\link{findmodel}}).
#' 
#'Please note, that  \code{robust} estimation with (Huber ability estimate) polytomous items is still experimental!
#' 
#'@param respm An integer matrix (or a data.frame), which contains the responses of the examinees. A persons x items matrix is expected. Missing responses are \code{NA}.
#'@param thres A numeric matrix which contains the threshold parameter for each item. If the first row of the matrix is not set to zero (only zeroes in the first row) - then a row-vector with zeroes is added by default.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected. A single value is used for all items. If \code{NULL} (default), all slopes are 1.
#'@param lowerA A numeric vector, which contains the lower asymptote parameters (kind of guessing parameter) for each item. It applies to the 4PL items only; a value other than 0 for a GPCM item is ignored with a warning. A single value is used for all items. If \code{NULL} (default), all lower asymptotes are 0.
#'@param upperA numeric vector, which contains the upper asymptote parameters for each item. It applies to the 4PL items only; a value other than 1 for a GPCM item is ignored with a warning. A single value is used for all items. If \code{NULL} (default), all upper asymptotes are 1.
#'@param theta_start A vector which contains a starting value for each person. If NULL is submitted, the starting values are set automatically. If a scalar is submitted, this start value is used for each person.
#'@param mu A numeric vector of location parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 0 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param sigma2 A numeric vector of variance parameters for each person, the normal prior for MAP and EAP estimation and for \code{\link{PV}}. A single value is used for all persons. If nothing is submitted this is set to 1 for each person. MLE, WLE and robust estimation have no prior: a value given there is ignored with a warning, and the result stores \code{NA}.
#'@param type Which maximization should be applied? There are five valid entries possible: "mle", "wle", "map", "eap" and "robust". To choose between the methods, or just to get a deeper understanding the papers mentioned below are quite helpful. The default is \code{"wle"} which is a good choice in many cases. With \code{"eap"} the estimate is the mean and the SE the standard deviation of the posterior, computed on a grid of ability values (see \link{PV}). Plausible values (\code{\link{PV}}) need the prior of \code{"eap"} or \code{"map"}.
#'
#'@param model2est No longer needed: the model of each item follows from \code{thres}. A value given is ignored with a warning. (A dichotomous item declared as \code{"GPCM"} is the same as a 4PL item with lower asymptote 0 and upper asymptote 1.)
#'
#'@param maxsteps The maximum number of Newton steps per person. Default = 100.
#'@param exac How accurate are the estimates supposed to be? The iteration of a person stops when the step is smaller than \code{exac}. Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and \code{H} modulates how fast the downweighting takes place (for more Details read Schuster & Yuan 2011).
#'@param ctrl No longer used. Its options \code{killdupli} and \code{skipcheck} of version 0.6.4 saved time with the old estimation routine; since 0.7.0 every person is estimated separately, and options given are ignored with a warning.
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
#'@examples
#' # items 1 to 3 are 4PL items (one threshold), items 4 to 6 GPCM items
#' thres <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
#' thres[2, 1:3] <- NA
#' sl <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
#' la <- c(0.02, 0.1, 0, 0, 0, 0)
#' ua <- c(0.97, 0.91, 1, 1, 1, 1)
#' set.seed(1811)
#' x <- sim_mixed(thres = thres, alpha = sl, lowerA = la, upperA = ua, theta = rnorm(30))
#'
#' res <- PPall(respm = x, thres = thres, slopes = sl, lowerA = la, upperA = ua, type = "wle")
#' summary(res)
#'
#' res_eap <- PPall(respm = x, thres = thres, slopes = sl, lowerA = la, upperA = ua, type = "eap")
#' head(res_eap$estimates)
#'@rdname PPall
#'
PPall <- function(respm, thres, slopes = NULL, lowerA = NULL, upperA = NULL, theta_start=NULL,
                  mu = NULL, sigma2 = NULL, type="wle", model2est = NULL,
                  maxsteps=100, exac=0.001, H=1, ctrl=list(), range = c(-6, 6), verbose = FALSE)
{
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  if (!is.null(model2est)) {
    warning("'model2est' is no longer needed and is ignored; the model of each item follows ",
            "from 'thres'.", call. = FALSE)
  }
  prep <- pp_prepare(respm, thres, slopes = slopes, lowerA = lowerA, upperA = upperA,
                     mu = mu, sigma2 = sigma2,
                     theta_start = theta_start, type = type, maxsteps = maxsteps,
                     exac = exac, H = H, ctrl = ctrl, range = range, verbose = verbose,
                     family = "mixed")
  pp_estimate(prep, call, "gpcm4pl",
              paste("mixed", paste(unique(prep$model2est), collapse = ", ")))
}


pp_status <- c("ok", "extreme", "boundary", "no_response", "not_converged")

# prep: result of pp_prepare(); call: the call of the wrapper; cls: "fourpl",
# "gpcm" or "gpcm4pl"; label: model description for the verbose message.
pp_estimate <- function(prep, call, cls, label) {
  X <- prep$X
  N <- nrow(X)
  type <- prep$type
  if (prep$verbose) message("Estimating: ", label, ", type = ", type)
  if (type == "robust" && prep$family != "4pl") {
    warning("Robust estimation for GPCM is still very experimental!", call. = FALSE)
  }

  if (type == "eap") {
    # posterior mean and SD on the grid; persons without answers get the prior
    post <- pp_posterior_cpp(prep$bank, X, prep$mu, prep$sigma2)
    res <- list(theta = post$eap, se = post$psd, nsteps = integer(N),
                status = ifelse(rowSums(!is.na(X)) == 0, 4L, 1L))
  } else {
    res <- pp_estimate_cpp(prep$bank, X, type, prep$mu, prep$sigma2, prep$H,
                           prep$start, prep$range, prep$exac, prep$maxsteps)
  }
  status <- factor(pp_status[res$status], levels = pp_status)

  nb <- sum(status == "boundary")
  nn <- sum(status == "not_converged")
  if (nb > 0) {
    warning(nb, " person(s) without a solution inside 'range' = c(", prep$range[1], ", ",
            prep$range[2], "): the estimate is set to the bound (status \"boundary\" in ",
            "$estimates).", call. = FALSE)
  }
  if (nn > 0) {
    warning(nn, " person(s) not converged within maxsteps = ", prep$maxsteps,
            " (status \"not_converged\" in $estimates).", call. = FALSE)
  }

  new_ppeo(prep, res$theta, res$se, res$nsteps, status, call, cls)
}

# The result object (class c(cls, "ppeo")):
#   estimates  data.frame estimate, SE, nsteps, status; one row per person in
#              the order of respm
#   type, call
#   model      the prepared inputs (pp_prepare())
#   resPP      list(resPP = n x 2 matrix estimate/SE, nsteps = max(nsteps)),
#              the result layout of 0.6.4, kept as an alias
new_ppeo <- function(prep, theta, se, nsteps, status, call, cls) {
  estimates <- data.frame(estimate = theta, SE = se, nsteps = as.integer(nsteps),
                          status = status)
  respp <- cbind(estimate = theta, SE = se)

  structure(list(estimates = estimates, type = prep$type, model = prep, call = call,
                 resPP = list(resPP = respp, nsteps = max(0L, nsteps))),
            class = c(cls, "ppeo"))
}
