pp_jk <- function(respm, thres, slopes, lowerA=NULL, upperA=NULL, theta_start=NULL,
                  mu = NULL, sigma2 = NULL, 
                  maxsteps=500, exac=0.001,ctrl=list(),
                  modest,model2est,estobj,cmeth="median")
{
# jackknife estimation  
  
  
loa <- 1:ncol(respm)
# save the responses into this matrix
jk_mat <- matrix(0,nrow=nrow(respm),ncol=length(loa))


for(jkrun in loa)
  {
    
    

  if(modest %in% c("2pl","3pl","4pl","3pl_upperA"))
    {
      jk_mat[,jkrun] <- PP:::NR_4PL(respm[,-jkrun],DELTA = thres[,-jkrun],ALPHA = slopes[-jkrun],
                               CS = lowerA[-jkrun],
                               DS = upperA[-jkrun], THETA = theta_start, 
                               wm=type,maxsteps,exac,mu,sigma2)$resPP[,1] 
    
    } else if(modest == "GPCM")
        {
          jk_mat[,jkrun] <- NR_GPCM(respm[,-jkrun], thres[,-jkrun],
                                    slopes[-jkrun], theta_start, type,
                                    maxsteps, exac, mu, sigma2)$resPP[,1]  
        } else 
            {
              jk_mat[,jkrun] <-  NR_mixed(respm[,-jkrun],DELTA = thres[,-jkrun],ALPHA = slopes[-jkrun],
                                          CS = lowerA[-jkrun], DS = upperA[-jkrun], 
                                          THETA = theta_start, wm=type,maxsteps,
                                          exac,mu,sigma2)$resPP[,1]
             }
    
  
  }

  
RES <- resPP$resPP[[1]][,1]



psvalues  <- RES *ncol(respm) - jk_mat * (ncol(respm) - 1)

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


retlist <- list(jkest=jkest,jk_mat=jk_mat)


return(retlist)

}




####### compute AMT
AMTnew <- function(TT,xj)
{
  suppressWarnings(zw <- sin((xj + TT)/2.1))
  nullk <- abs(xj) < 2.1*pi
  zw[!nullk] <- 0
  
  sum(zw)
}





