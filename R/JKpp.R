#' Run a jackknife
#' 
#' This function uses a jackknife approach to compute person parameters.
#' 
#' @param estobj An object which contains a conventional Person Parameter estimation
#' @param ... Three Points
#' @export
#' @rdname Jkpp
JKpp <- function(estobj,...) UseMethod("JKpp")


# ---------------------------------------------------------------------


#' @rdname Jkpp
#' @param cmeth Centering method, to summarize the jackknife results to one single ability estimate
#' @param maxsteps The maximum number of steps the NR Algorithm will take.
#' @param exac How accurate are the estimates supposed to be? Default is 0.001.
#' @param ctrl more controls
#' @method JKpp fourpl
#' 
JKpp.fourpl <- function(estobj, cmeth="mean", maxsteps=500, exac=0.001,
                     ctrl=list())
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
  

  
type <- estobj$type 
loa <- 1:ncol(respm)
# save the responses into this matrix
jk_mat <- matrix(0,nrow=nrow(respm),ncol=length(loa))

  
# run the 4pl jackknife  
for(jkrun in loa)
{
  

jk_mat[,jkrun] <- NR_4PL(respm[,-jkrun],DELTA = thres[,-jkrun],ALPHA = slopes[-jkrun],
                              CS = lowerA[-jkrun],DS = upperA[-jkrun], THETA = theta_start, 
                              wm=type,maxsteps,exac,mu,sigma2)$resPP[,1] 
    
}

  
  
RES <- estobj$resPP$resPP[,1]



psvalues  <- RES *ncol(respm) - jk_mat * (ncol(respm) - 1)
  
output_jk <- cco(psvalues) 

output_jk  
}







# ---------------------------------------------------------------------

#' @rdname Jkpp
#' @method JKpp gpcm
#' 
JKpp.gpcm <- function(estobj, cmeth="mean", maxsteps=500, exac=0.001,
                     ctrl=list())
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
  
  
  
  RES <- estobj$resPP$resPP[,1]
  
  
  
  psvalues  <- RES *ncol(respm) - jk_mat * (ncol(respm) - 1)
  
  output_jk <- cco(psvalues) 
  
  output_jk  
}





# ---------------------------------------------------------------------

#' @rdname Jkpp
#' @method JKpp gpcm4pl
#' 
JKpp.gpcm4pl <- function(estobj, cmeth="mean", maxsteps=500, exac=0.001,
                      ctrl=list())
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
    
  
  
  RES <- estobj$resPP$resPP[,1]
  
  psvalues  <- RES *ncol(respm) - jk_mat * (ncol(respm) - 1)
  
  output_jk <- cco(psvalues) 
  
  output_jk  
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

cco <- function(psvalues)
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












