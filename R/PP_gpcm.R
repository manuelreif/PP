#' Estimate Person Parameters for the GPCM
#' 
#' Compute Person Parameters for the GPCM and choose between five common estimation techniques: MLE, WLE, MAP, EAP and a robust estimation. All item parameters are treated as fixed.
#'
#' There are not Details up to now.
#' 
#'@param respm An integer matrix, which contains the examinees reponses. An Persons x items matrix is expected.
#'@param thres An numeric matrix which contains the threshold parameter for each item. If the first row of the matrix is not set to zero (only zeroes in the first row) - then a row-vector with zeroes is added by default.
#'@param slopes A numeric vector, which contains the slope parameters for each item - one parameter per item is expected.
#'@param theta_start A vector which contains a starting value for each person. Currently this is necessary to supply, but soon it will be set automatically if nothing is committed.
#'@param mu A numeric vector of location parameters for each person in case of MAP estimation. If nothing is submitted this is set to 0 for each person for map estimation.
#'@param sigma2 A numeric vector of variance parameters for each person in case of MAP estimation. If nothing is submitted this is set to 1 for each person for map estimation.
#'@param type There are three valid entries possible: "mle", "wle" or "map". "wle" is recommanded. For a deeper understanding the papers mentioned below would be helpful for sure. 
#'@param maxsteps The maximum number of steps the NR Algorithm will take.
#'@param exac How accurate are the estimates supposed to be? Default is 0.001.
#'@param H In case \code{type = "robust"} a Huber ability estimate is performed, and H modulates how fast the downweighting takes place.
#'@param ctrl more controls
#'\itemize{
#' \item \code{killdupli} Should duplicated response pattern be removed for estimation (estimation is faster)? This is especially resonable in case of a large number of examinees and a small number of items.  Use this option with caution (for map and eap), because persons with different \code{mu} and \code{sigma2} will have different ability estimates despite they responded identically. Default value is \code{FALSE}.
#'
#'}
#'
#'@template resulttemplate
#'
#' @seealso \link{PPall}, \link{PP_4pl}, \link{JKpp}
#'
#'@export
#'
#'@author Manuel Reif
#'@references Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory - Parameter Estimation Techniques. CRC-Press.
#'
#'Muraki, Eiji (1992). A Generalized Partial Credit Model: Application of an EM Algorithm. Applied Psychological Measurement, 16, 159-176.
#'
#'Muraki, Eiji (1993). Information Functions of the Generalized Partial Credit Model. Applied Psychological Measurement, 17, 351-363.
#'
#'Samejima, Fumiko (1993). The bias function of the maximum likelihood estimate of ability for the dichotomous response level. Psychometrika,  58, 195-209.
#'
#'Samejima, Fumiko (1993). An approximation of the bias function of the maximum likelihood estimate of a latent variable for the general case where the item responses are discrete. Psychometrika,  58, 119-138.
#'
#'Wang, S. and Wang, T. (2001). Precision of Warm's Weighted Likelihood Estimates for a Polytomous Model in Computerized Adaptive Testing. Applied Psychological Measurement, 25, 317-331.
#'
#'Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In Item Response Theory. Psychometrika, 54, 427-450.
#'

#'@example ./R/.examples_gpcm.R
#'@keywords Person Parameters, GPCM
#'@rdname PP_gpcm
#'




PP_gpcm <- function(respm, thres, slopes, theta_start=NULL,
                    mu = NULL, sigma2 = NULL, type="wle", maxsteps=100, exac=0.001,H=1,ctrl=list())
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
  
  
  ## --------- starting values
  
  if(is.null(theta_start))
  {
    theta_start <- rep(0,nrow(respm))
  }
  
  #---
  
#   if(cont$cdiag) 
#   {
#     cont$killdupli <- FALSE
#     warning("killdupli in 'ctrl' is forced to FALSE!\n")  
#   }
  


## --------- threshold matrix

# If the user did not add a row with zeroes only
  if(any(thres[1,] != 0))
    {
      thres <- rbind(0,thres)
    }
  
  # compute the maximal score per item
  maxsc <- apply(thres,2,function(x)(length(x) - sum(is.na(x)))-1)
  

  

  
## --------- check user inputs
match.arg(type,c("mle","wle","map","eap","robust"))

if(length(type) != 1) stop("Submit a single value as 'type'!\n")


if(!is.matrix(respm)) stop("respm must be a matrix!\n")
if(!is.matrix(thres)) stop("thres must be a matrix!\n")

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

  
  
# ----- ----- -------------#


# add NA and Inf in case of mle estimation and full oder 0 score
if(type=="mle")
{
  resPPx <- ansol(respm,maxsc)  
  respm <- respm[!is.na(resPPx[,2]),]
}



# ----- remove duplicated -------------# 

if(cont$killdupli)
{
  dupvec <- make_dup(respm)
  respm <- respm[dupvec$ndpat,]
}


cat("Estimating: GPCM ... \n")
cat("type =",type,"\n")


# 
# if(type %in% c("mle","wle","map"))
# {
#   
#   resPP <- NR_4PL(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start, wm=type,maxsteps,exac,mu,sigma2,H=H)
#   
# } else if(type == "robust")
# {
#   
#   resPP <- NR_4PL(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start, wm=type,maxsteps,exac,mu,sigma2,H=H)
#   
# } else if(type == "eap")
# {
#   resPP <- list()
#   resPP$resPP <- eap_4pl(respm, thres, slopes, lowerA=lowerA, upperA=upperA,
#                          mu = mu, sigma2 = sigma2)
#   resPP$nsteps <- 0
# }


if(type %in% c("mle","wle","map"))
  {
    # ----- estimation procedure -------------# 
    
    resPP <- NR_GPCM(respm,thres,slopes,theta_start,type,maxsteps,exac,mu,sigma2,H=H) 
  } else if(type == "robust")
    {
    warning("Robust estimation for GPCM is still very experimental! \n")
    resPP <- NR_GPCM(respm,thres,slopes,theta_start,type,maxsteps,exac,mu,sigma2,H=H) 
      
    } else if(type == "eap")
      {
        resPP <- list()
        resPP$resPP <- eap_gpcm(respm, thres, slopes,
                                mu = mu, sigma2 = sigma2)
        resPP$nsteps <- 0    
      }


    
  ### result preperation --------------------------
  
  # add duplicated values
  if(cont$killdupli)
    {
      # expanding up the matrix
      resPP$resPP <- resPP$resPP[dupvec$posvec,]
    }
  
  # add Inf and -Inf 
  if(type=="mle")
    {
      resPPx[!is.na(resPPx[,2]),] <- resPP$resPP
      resPP$resPP <- resPPx
    }
  
  colnames(resPP$resPP) <- c("estimate","SE")
  

  ipar <- list(respm=respm,thres=thres,slopes=slopes,theta_start=theta_start,mu=mu,sigma2=sigma2,cont=cont)
  
  
  if(cont$killdupli)
    {
      ipar$dupvec <- dupvec 
    }

  ## ---------------------------------------------
  cat("Estimation finished!\n")
  rescall <- list(resPP=resPP,call=call,type=type,ipar=ipar)
  class(rescall) <- c("gpcm","ppeo")

  
  
  
return(rescall)  
  
}