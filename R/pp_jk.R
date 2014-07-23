pp_jk <- function(respm, thres, slopes, lowerA=NULL, upperA=NULL, theta_start=NULL,
                  mu = NULL, sigma2 = NULL, type="wle", 
                  maxsteps=500, exac=0.001,ctrl=list(),
                  modest,model2est,resPP)
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

  
resPP <- test3$resPP 

rowMeans(resPP$resPP[,1] *ncol(awm) - jk_mat * (ncol(awm) - 1),na.rm=TRUE)
jk_mat

}


