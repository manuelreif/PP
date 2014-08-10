#' Estimate Person Parameters for the 4pl model
#' 
#' Compute Person Parameters for the 1/2/3/4pl model and choose between five common estimation techniques: MLE, WLE, MAP, EAP and a robust estimation. All items parameters are treated as fixed.
#'
#' blabla
#' 
#'@param respm An integer matrix, which contains the examinees reponses. An Persons x items matrix is expected.
#'@param thres An numeric vector or a numeric matrix which contains the threshold parameter for each item. If a matrix is submitted, the first row must contain only \bold{zeroes}!
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected.
#'@param lowerA A numeric vector, which contains the lower asymptote parameters (kind of guessing parameter) for each item.
#'@param upperA numeric vector, which contains the upper asymptote parameters for each item.
#'@param theta_start A vector which contains a starting value for each person. Currently this is necessary to supply, but soon it will be set automatically if nothing is committed.
#'@param mu A numeric vector of location parameters for each person in case of MAP estimation. If nothing is submitted this is set to 0 for each person for map estimation.
#'@param sigma2 A numeric vector of variance parameters for each person in case of MAP estimation. If nothing is submitted this is set to 1 for each person for map estimation.
#'@param type There are three valid entries possible: "mle", "wle" or "map". "wle" is recommanded. For a deeper understanding the papers mentioned below would be helpful for sure. 
#'@param maxsteps The maximum number of steps the NR Algorithm will take.
#'@param exac How accurate are the estimates supposed to be? Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and H modulates how fast the downweighting takes place. 
#'@param ctrl more controls:
#'\itemize{
#' \item \code{killdupli} Should duplicated response pattern be removed for estimation (estimation is faster)? This is especially resonable in case of a large number of examinees and a small number of items.  Use this option with caution (for map and eap), because persons with different \code{mu} and \code{sigma2} will have different ability estimates despite they responded identically. Default value is \code{FALSE}.
#'
#'}
#'
#'@template resulttemplate
#'
#' @seealso \link{PPall}, \link{PP_gpcm}, \link{JKpp}
#'
#' @useDynLib PP
#' @importFrom Rcpp evalCpp
#'
#'@export
#'
#'@author Manuel Reif
#'@references 
#'Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory - Parameter Estimation Techniques. CRC-Press.
#'
#'Magis, D. (2013). A note on the item information function of the four-parameter logistic model. Applied Psychological Measurement, 37(4), 304-315.
#'
#'Muraki, Eiji (1993). Information Functions of the Generalized Partial Credit Model. Applied Psychological Measurement, 17, 351-363.
#'
#'Samejima, Fumiko (1993). The bias function of the maximum likelihood estimate of ability for the dichotomous response level. Psychometrika,  58, 195-209.
#'
#'Samejima, Fumiko (1993). An approximation of the bias function of the maximum likelihood estimate of a latent variable for the general case where the item responses are discrete. Psychometrika,  58, 119-138.
#'
#'Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In Item Response Theory. Psychometrika, 54, 427-450.
#'
#'Yen, Y.-C., Ho, R.-G., Liao, W.-W., Chen, L.-J., & Kuo, C.-C. (2012). An empirical evaluation of the slip correction in the four parameter logistic models with computerized adaptive testing. Applied Psychological Measurement, 36, 75-87.
#'

#'@example ./R/.examples_4pl.R
#'@keywords Person Parameters, 4pl
#'@rdname PP_4pl
#'



PP_4pl <- function(respm, thres, slopes, lowerA=NULL, upperA=NULL, theta_start=NULL,
                   mu = NULL, sigma2 = NULL, type="wle", maxsteps=40, exac=0.001,H=1,ctrl=list())
{
  
  ### 
  call <- match.call()  
  attr(call, "date") <- date() 
  attr(call,"version") <- packageVersion("PP")
  ###
  
  
## --------- user controls
cont <- list(killdupli=FALSE)

user_ctrlI <- match(names(ctrl),names(cont))

if(any(is.na(user_ctrlI)))
  {
    notex <- names(ctrl)[is.na(user_ctrlI)]
    warning("The following options in ctrl do not exist: ", paste(notex,collapse=", "))
    ctrl       <- ctrl[!is.na(user_ctrlI)]
    user_ctrlI <- user_ctrlI[!is.na(user_ctrlI)]
  }

  cont[user_ctrlI] <- ctrl
  
  
  ## starting values
  
if(is.null(theta_start))
  {
    theta_start <- rep(0,nrow(respm))
  }
  
  #---
  
#   if(cont$cdiag) 
#     {
#       cont$killdupli <- FALSE
#       warning("killdupli in 'ctrl' is forced to FALSE!\n")  
#     }
  
  

## --------- threshold matrix
  
## take care of the threshold 'matrix' - for the 4PL model
# a thres-vector is allowed - but for the internal routines
# it had to be reshaped as a matrix

  
if(is.matrix(thres))
  {
    #iimm <- nrow(thres) == 1
    
    if(any(thres[1,] != 0))
      {
       thres <- rbind(0,thres)
      }
    
    # compute the maximal score per item
    maxsc <- apply(thres,2,function(x)(length(x) - sum(is.na(x)))-1)
    
    # are there any items with more than 2 categories?
    #allebigger <- any(maxsc > 1)
    
  } else if(is.vector(thres))
      {
        #iimm <- TRUE 
        thres <- rbind(0,thres)
        #allebigger <- FALSE
        maxsc <- apply(thres,2,function(x)(length(x) - sum(is.na(x)))-1)
      }



  
## --------- check user inputs
match.arg(type,c("mle","wle","map","eap","robust"))

if(length(type) != 1) stop("Submit a single value as 'type'!\n")

# in case map is chosen and no mu and/or sigma2 is/are submitted.
if( (any(is.null(mu)) | any(is.null(sigma2))))
  {
  if(any(is.null(mu))) 
    {
    mu <- rep(0,nrow(respm))
    if(type %in% c("map","eap")) warning("all mu's are set to 0! \n")
    }
  
  if(any(is.null(sigma2))) 
    {
    sigma2 <- rep(1,nrow(respm))
    if(type %in% c("map","eap")) warning("all sigma2's are set to 1! \n")
    }
  
  }

  ## ----------------------
  
  
  
# ----- CHOOSE MODEL -------------#


if(is.null(lowerA) & is.null(upperA)) 
{modest <- "2pl"} else if(is.null(upperA))
{modest <- "3pl"} else if(is.null(lowerA))
{modest <- "3pl_upperA"} else 
{modest <- "4pl"}


cat("Estimating: ",modest,"model ... \n")
cat("type =",type,"\n")

  
  
  #### conditional controls
  
  if(modest == "4pl")
  {
    if(!all(length(slopes) == c(length(lowerA),ncol(respm),length(upperA),ncol(thres)))) stop("Check length of sumitted vectors!\n")  
  } else if(modest == "3pl")
    {
      if(!all(length(slopes) == c(length(lowerA),ncol(respm),ncol(thres)))) stop("Check length of sumitted vectors!\n")  
    } else if(modest == "2pl")
      {
        if(!all(length(slopes) == c(ncol(respm),ncol(thres)))) stop("Check length of sumitted vectors!\n")  
      } else if(modest == "3pl_upperA")
        {
          if(!all(length(slopes) == c(ncol(respm),ncol(thres),length(upperA)))) stop("Check length of sumitted vectors!\n")  
        }
    
  
  
  # ----- ----- -------------#
  
  
  # add NA and Inf in case of mle estimation and full oder 0 score
  if(type=="mle")
  {
    resPPx <- ansol(respm,maxsc)  
    respm <- respm[!is.na(resPPx[,2]),]
  }
  
  
  
  ## kill duplicated if desired #################
  
  if(cont$killdupli)
  {
    # this vector contains the indices, to recreate the large matrix in the end of the estimation
    dupvec <- make_dup(respm)
    respm <- respm[dupvec$ndpat,]
  }
  
  
  ##############################################
  
  
  
  
  
  # ----- estimation procedure -------------# 
  

# prepare not submitted vectors

      if(modest == "2pl")
        {
          lowerA <- rep(0,length(slopes))  
          upperA <- rep(1,length(slopes))  
        } else if(modest == "3pl")
          {
            upperA <- rep(1,length(slopes))  
          } else if(modest == "3pl_upperA")
            {
              lowerA <- rep(0,length(slopes))    
            }
        
if(type %in% c("mle","wle","map"))
  {
    
  resPP <- NR_4PL(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start, wm=type,maxsteps,exac,mu,sigma2,H=H)
    
  } else if(type == "robust")
    {
    
    resPP <- NR_4PL(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start, wm=type,maxsteps,exac,mu,sigma2,H=H)
    
    } else if(type == "eap")
        {
          resPP <- list()
          resPP$resPP <- eap_4pl(respm, thres, slopes, lowerA=lowerA, upperA=upperA,
                         mu = mu, sigma2 = sigma2)
          resPP$nsteps <- 0
        }
        
      

  
### result preperation --------------------------

if(cont$killdupli)
  {
   resPP$resPP <- resPP$resPP[dupvec$posvec,]
  }

if(type=="mle")
  {
   resPPx[!is.na(resPPx[,2]),] <- resPP$resPP
   resPP$resPP <- resPPx
  }

## ---------------------------------------------

colnames(resPP$resPP) <- c("estimate","SE")

ipar <- list(respm=respm,thres=thres,slopes=slopes,lowerA=lowerA,
             upperA=upperA,theta_start=theta_start,mu=mu,sigma2=sigma2,cont=cont)


if(cont$killdupli)
  {
  ipar$dupvec <- dupvec 
  }

## ---------------------------------------------
cat("Estimation finished!\n")
rescall <- list(resPP=resPP,call=call,type=type,ipar=ipar)
class(rescall) <- c("fourpl","ppeo")



return(rescall)
  
  

  
  
  
}