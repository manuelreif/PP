#' Simulated data set
#' 
#' This dataset contains the responses of 60 persons to 12 items of a 3PL test, simulated with the \code{sim_4pl()} function, together with an id and the sex of each person. The item parameters are stored as attributes.
#'
#' @format A data.frame with 60 rows and 14 columns:
#' \describe{
#' \item{\code{id}}{person id}
#' \item{\code{sex}}{sex of the person (\code{"m"} or \code{"w"})}
#' \item{\code{Item1}, \ldots, \code{Item12}}{the responses (0 or 1, \code{NA} = not answered)}
#' }
#' The item parameters are the attributes \code{diffpar} (difficulties), \code{slopes}, \code{lowerA} (lower asymptotes) and \code{upperA} (upper asymptotes, all 1), e.g. \code{attr(fourpl_df, "diffpar")}.
#' @source simulation
#' @seealso \link{sim_4pl}
#' @name fourpl_df
NULL

#' Adaptive Matrices Test data
#' 
#' This dataset contains real data from the 'Adaptive Matrices Test' (AMT), which is a computer-administered test.
#' This power test assesses logical reasoning as an indicator of general intelligence. The ability to identify regularities and draw logical conclusions is a very good predictor of long-term success at work. The dataset is sparse, because the test tailors a specific set of items for each examinee's ability level. (More information about adaptive testing: \url{https://en.wikipedia.org/wiki/Computerized_adaptive_testing})
#'
#' 
#' The data are provided from the University of Vienna, Faculty of Psychology, Department of Psychological Assessment.
#' Thanks to Schuhfried.
#' 
#' @format A list with two data.frames. The first data.frame 'daten_amt' contains 298 columns and 710 rows. Each row contains responses from one examinee. The second data.frame 'betas' contains the difficulty parameter (1PL) (These parameters came with the raw-score extraction.).
#' @source Division of Psychological Assessment and Applied Psychometrics, Faculty of Psychology, University of Vienna
#' \itemize{
#' \item ID: id of person
#' \item AGE: age in years (with ages below 18 and above 34 are collapsed)
#' \item TE_GA:
#' \itemize{
#' \item TE: self-assessment. To pass a psychological assessment course, the students have to complete several hours self assessment on a bunch of tests, to get familiar with them.
#' \item GA: testing for an assessment report. The students also have to test other people (not psychologists nor psychology students) in order to write an assessment report.
#' }
#' 
#' 
#' \item FORM: There are several different versions of this test, which differ in test length, duration etc \ldots
#' \item TIME1: start time
#' \item TIME2: end time
#' \item REL: reliability for each person
#' \item i: items
#' }
#' @references 
#' \itemize{
#' \item Hornke, L. F., Etzel, S., & Rettig, K. (2003). Manual Adaptive Matrices Test (AMT). \emph{Moedling: SCHUHFRIED GmbH}.
#' }
#' 
#' @seealso \link{PPass}
#' @name pp_amt
NULL
