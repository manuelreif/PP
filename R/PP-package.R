#' PP: Person Parameter Estimation
#'
#' The PP package estimates person parameters (abilities) in item response
#' theory when the item parameters are known and fixed, e.g. for items from an
#' item bank given to a new sample of persons.
#'
#' \describe{
#' \item{Models}{the 1PL (Rasch), 2PL, 3PL and 4PL model (\code{\link{PP_4pl}}),
#' the partial credit and generalized partial credit model (\code{\link{PP_gpcm}})
#' and tests that mix both (\code{\link{PPall}}).}
#' \item{Estimates}{maximum likelihood (MLE), weighted likelihood (WLE; Warm,
#' 1989), maximum a posteriori (MAP), expected a posteriori (EAP) and a robust
#' Huber-type estimate, each with a standard error and a status per person.}
#' \item{Further analyses}{jackknife estimates and standard errors
#' (\code{\link{JKpp}}), plausible values (\code{\link{PV}}) and the person fit
#' indices lz, lz*, infit and outfit (\code{\link{Pfit}}).}
#' \item{In one step}{\code{\link{PPass}} estimates and computes the person fit
#' for a data.frame with item responses and other person variables, or for a
#' model fitted with the eRm package (RM, LLTM, RSM, PCM, LRSM, LPCM).}
#' \item{Simulation}{\code{\link{sim_4pl}}, \code{\link{sim_gpcm}} and
#' \code{\link{sim_mixed}}.}
#' }
#'
#' The numerical core is written in C++. Every person is estimated with a
#' safeguarded Newton method, EAP estimates and plausible values come from the
#' posterior on a grid. The vignettes give an introduction:
#' \code{vignette("intro_pp", package = "PP")},
#' \code{vignette("intro_pfit", package = "PP")} and
#' \code{vignette("ppass_erm", package = "PP")}.
#'
#' @references
#' Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
#' three-parameter logistic item-response model. \emph{ETS Research Report
#' Series}, 1981(1), i--8.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring an
#' examinee's ability. In F. M. Lord & M. R. Novick (Eds.), \emph{Statistical
#' theories of mental test scores}. Reading, MA: Addison-Wesley.
#'
#' Drasgow, F., Levine, M. V., & Williams, E. A. (1985). Appropriateness
#' measurement with polychotomous item response models and standardized indices.
#' \emph{British Journal of Mathematical and Statistical Psychology},
#' \bold{38}(1), 67--86.
#'
#' Muraki, E. (1992). A generalized partial credit model: Application of an EM
#' algorithm. \emph{Applied Psychological Measurement}, \bold{16}, 159--176.
#'
#' Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability in
#' item response models. \emph{Journal of Educational and Behavioral
#' Statistics}, \bold{36}(6), 720--735.
#'
#' Snijders, T. B. (2001). Asymptotic null distribution of person fit statistics
#' with estimated person parameter. \emph{Psychometrika}, \bold{66}(3), 331--342.
#'
#' Wainer, H., & Wright, B. D. (1980). Robust estimation of ability in the Rasch
#' model. \emph{Psychometrika}, \bold{45}(3), 373--391.
#'
#' Warm, T. A. (1989). Weighted likelihood estimation of ability in item
#' response theory. \emph{Psychometrika}, \bold{54}, 427--450.
#'
#' @examples
#' # 12 items of a 2PL test with known parameters, 100 persons
#' set.seed(1522)
#' b  <- seq(-3, 3, length = 12)
#' sl <- round(runif(12, 0.5, 1.5), 2)
#' x  <- sim_4pl(beta = b, alpha = sl, lowerA = 0, upperA = 1, theta = rnorm(100))
#'
#' res <- PP_4pl(respm = x, thres = b, slopes = sl, type = "wle")
#' head(res$estimates)
#'
#' # estimates and person fit in one table
#' PPass(data.frame(id = 1:100, x), items = 2:13, mod = "2PL", thres = b, slopes = sl)
#'
#' @useDynLib PP, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats median qnorm sd setNames
#' @importFrom utils head packageVersion
"_PACKAGE"
