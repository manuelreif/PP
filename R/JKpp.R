#' Run a jackknife
#' 
#' This function uses a jackknife approach to compute person parameters.
#' 
#' Please use the Jackknife Standard-Error output with caution. It is implemented as suggested in the mentioned paper, but the results seem a bit strange, because the JK-SE is supposed to overestimate the SE compared to the MLE-SE. This was not the case for different examples! 
#' 
#' 
#' AMT-robustified jackknife: When choosing AMT, the single jackknife-ability estimates and the original ability estimate are combined to a single jackknife-ability value. The AMT (or Sine M-estimator) is one of the winners in the Princeton Robustness Study of 1972. To get a better idea how the estimation process works, take a closer look to the paper which is mentioned below (Wainer & Wright, 1980).
#' 
#' @param estobj An object which originates from using \code{PP_gpcm()}, \code{PP_4pl()} or \code{PPall()}.
#' 
#' @param ...  Three points!
#' @export
#' @rdname Jkpp
#' @references
#' Wainer, H., & Wright, B. D. (1980). Robust estimation of ability in the Rasch model. Psychometrika, 45(3), 373-391.
#'
#'
#' @seealso \link{PP_gpcm}, \link{PP_4pl}, \link{PPall}
#' 
#'@example ./R/.examples_JK.R
#' 
JKpp <- function(estobj,...) UseMethod("JKpp")


# ---------------------------------------------------------------------


#' @rdname Jkpp
#' @param cmeth Choose the centering method, to summarize the n jackknife results to one single ability estimate. There are three valid entries: "mean", "median" and "AMT".
#' @param maxsteps The maximum number of steps the NR Algorithm will take.
#' @param exac How accurate are the estimates supposed to be? Default is 0.001.
#' @param ctrl more controls
#' @method JKpp fourpl
#' @export
JKpp.fourpl <- function(estobj, cmeth="mean", maxsteps=500,
                        exac=0.001, ctrl=list(), ...)
{
  

# pick out objects  
respm <- estobj$ipar$respm
thres <- estobj$ipar$thres
slopes <- estobj$ipar$slopes
lowerA <- estobj$ipar$lowerA
upperA <- estobj$ipar$upperA
theta_start <- estobj$ipar$theta_start
mu <- estobj$ipar$mu
sigma2 <- estobj$ipar$sigma2
cont <- estobj$ipar$cont
H <- estobj$ipar$H

# in case the killdupli option = TRUE 
## if not, create a vector which indicates merely each row
if(!is.null(estobj$ipar$dupvec$posvec))
  {
    
  POS <- estobj$ipar$dupvec$posvec
  } else 
    {
      POS <- 1:nrow(respm)  
    }
    
type <- estobj$type 
loa <- 1:ncol(respm)
# save the responses into this matrix
jk_mat <- matrix(0,nrow=nrow(respm),ncol=length(loa))

  
# run the 4pl jackknife  
for(jkrun in loa)
  {
    
  
  jk_mat[,jkrun] <- NR_4PL(respm[,-jkrun],DELTA = thres[,-jkrun],ALPHA = slopes[-jkrun],
                                CS = lowerA[-jkrun],DS = upperA[-jkrun], THETA = theta_start, 
                                wm=type,maxsteps,exac,mu,sigma2,H=H)$resPP[,1] 
      
  }

notna <- !is.na(estobj$resPP$resPP[,2])

RES <- estobj$resPP$resPP[notna,1]

psvalues  <- RES *ncol(respm) - jk_mat[POS,] * (ncol(respm) - 1)

output_jk <- cco(psvalues,cmeth) 

### jk standard errors - see page 388
L <- ncol(respm)
Lm1 <- L - 1

jk_se <- sqrt(rowSums((jk_mat[POS,] - output_jk)^2,na.rm=TRUE)/(L*Lm1))


resjk_raw <- cbind(output_jk,jk_se)

if(cont$killdupli)
  {
    resjk_raw <- resjk_raw[POS,]
  }
  

resjk <- estobj$resPP$resPP
resjk[notna,] <- resjk_raw
  
return(resjk)

}







# ---------------------------------------------------------------------

#' @rdname Jkpp
#' @method JKpp gpcm
#' @export
JKpp.gpcm <- function(estobj, cmeth="mean", maxsteps=500, 
                      exac=0.001,  ctrl=list(), ...)
{
  
  
  # pick out objects  
  respm <- estobj$ipar$respm
  thres <- estobj$ipar$thres
  slopes <- estobj$ipar$slopes
#   lowerA <- estobj$ipar$lowerA
#   upperA <- estobj$ipar$upperA
  theta_start <- estobj$ipar$theta_start
  mu <- estobj$ipar$mu
  sigma2 <- estobj$ipar$sigma2
  cont <- estobj$ipar$cont
  
if(!is.null(estobj$ipar$dupvec$posvec))
  {
    
    POS <- estobj$ipar$dupvec$posvec
  } else 
    {
      POS <- 1:nrow(respm)  
    }

# skip -inf   

  type <- estobj$type 
  loa <- 1:ncol(respm)
  # save the responses into this matrix
  jk_mat <- matrix(0,nrow=nrow(respm),ncol=length(loa))
  
  
  # run the 4pl jackknife  
  for(jkrun in loa)
  {
    
    
    jk_mat[,jkrun] <- NR_GPCM(respm[,-jkrun], thres[,-jkrun],
                              slopes[-jkrun], theta_start, type,
                              maxsteps, exac, mu, sigma2)$resPP[,1]  
    
  }
  
  notna <- !is.na(estobj$resPP$resPP[,2])
  
  RES <- estobj$resPP$resPP[notna,1]
  
  psvalues  <- RES *ncol(respm) - jk_mat[POS,] * (ncol(respm) - 1)
  
  output_jk <- cco(psvalues,cmeth) 
  
  ### jk standard errors - see page 388
  L <- ncol(respm)
  Lm1 <- L - 1
  
  jk_se <- sqrt(rowSums((jk_mat[POS,] - output_jk)^2,na.rm=TRUE)/(L*Lm1))
  
  resjk_raw <- cbind(output_jk,jk_se)
  
  if(cont$killdupli)
    {
      resjk_raw <- resjk_raw[POS,]
    }

  
  resjk <- estobj$resPP$resPP
  resjk[notna,] <- cbind(output_jk,jk_se)
  
  return(resjk)

}





# ---------------------------------------------------------------------

#' @rdname Jkpp
#' @method JKpp gpcm4pl
#' @export 
JKpp.gpcm4pl <- function(estobj, cmeth="mean", maxsteps=500,
                         exac=0.001, ctrl=list(), ...)
{
  
  
  # pick out objects  
  respm  <- estobj$ipar$respm
  thres  <- estobj$ipar$thres
  slopes <- estobj$ipar$slopes
  lowerA <- estobj$ipar$lowerA
  upperA <- estobj$ipar$upperA
  theta_start <- estobj$ipar$theta_start
  mu <- estobj$ipar$mu
  sigma2 <- estobj$ipar$sigma2
  cont <- estobj$ipar$cont

  if(!is.null(estobj$ipar$dupvec$posvec))
    {
      
      POS <- estobj$ipar$dupvec$posvec
    } else 
      {
        POS <- 1:nrow(respm)  
      }
  
  
  type <- estobj$type 
  loa <- 1:ncol(respm)
  # save the responses into this matrix
  jk_mat <- matrix(0,nrow=nrow(respm),ncol=length(loa))
  
  
  # run the 4pl jackknife  
  for(jkrun in loa)
    {
      
      jk_mat[,jkrun] <- NR_GPCM(respm[,-jkrun], thres[,-jkrun],
                                slopes[-jkrun], theta_start, type,
                                maxsteps, exac, mu, sigma2)$resPP[,1]  
      
    }
    
  

  notna <- !is.na(estobj$resPP$resPP[,2])
  
  RES <- estobj$resPP$resPP[notna,1]
  
  psvalues  <- RES *ncol(respm) - jk_mat[POS,] * (ncol(respm) - 1)
  
  output_jk <- cco(psvalues,cmeth) 
  
  ### jk standard errors - see page 388
  L <- ncol(respm)
  Lm1 <- L - 1
  
  jk_se <- sqrt(rowSums((jk_mat[POS,] - output_jk)^2,na.rm=TRUE)/(L*Lm1))
  
  resjk_raw <- cbind(output_jk,jk_se)
  
  if(cont$killdupli)
    {
      resjk_raw <- resjk_raw[POS,]
    }
    
  
  resjk <- estobj$resPP$resPP
  resjk[notna,] <- cbind(output_jk,jk_se)
  
  return(resjk)
  
}














####### compute AMT
AMTnew <- function(TT,xj)
{
  suppressWarnings(zw <- sin((xj + TT)/2.1))
  nullk <- abs(xj) < 2.1*pi
  zw[!nullk] <- 0
  
  sum(zw)
}


###### compute composed PP

cco <- function(psvalues,cmeth)
{

  if(cmeth=="mean")
  {
    
    jkest <- rowMeans(psvalues,na.rm=TRUE)
    
  } else if(cmeth=="median")
  {
    jkest <- apply(psvalues,1,function(cen) median(cen,na.rm=TRUE))
    
  } else if(cmeth=="AMT")
  {
    
    jkest <- apply(psvalues,1,function(psv)
    {
      negpv <- psv[psv <= 0]
      pospv <- psv[psv > 0]
      
      if(all(is.nan(psv)))
      {
        yes <- NaN
      } else 
      {
        # wir haben hier bei MLE noch ein problem wenn NaN in psvalues enthalten sind. dasselbe problem gilt auch fuer wle, nur wird das hier wesentlich seltener vorkommen.
        an1 <- optim(0,AMTnew,xj=pospv,method="BFGS",control = list(fnscale=-1)) # minimize
        an2 <- optim(0,AMTnew,xj=negpv,method="BFGS",control = list(fnscale=1)) # maximize
        
        yes <- sum(an1$par*length(pospv) + an2$par*length(negpv))/length(psv)
        yes
        
      }
      
    })
    
  }    
  
return(jkest)  
}












