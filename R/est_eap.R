est_eap <- function(respm, thres, slopes, lowerA=NULL, upperA=NULL,
                    mu = NULL, sigma2 = NULL,npv=NULL,approx=TRUE)
{
  
  
nodmat <- matrix(0,nrow(respm),90)  
weimat <- matrix(0,nrow(respm),90) 
# #persons x #nodes

for(qrun in 1:nrow(respm))
  {
  qres <- quadIT(nodes = 90,mu = mu[qrun],sigma = sqrt(sigma2[qrun]),absrange = 6)
  nodmat[qrun,]  <- qres[[1]]$nodes
  weimat[qrun,]  <- qres[[1]]$weights
  }


FPL_eap <- sapply(1:nrow(respm),function(ep)
            {
              
            oben <- exp(t(outer(nodmat[ep,],thres[-1,],"-"))*slopes)
            oben2 <- oben * (upperA-lowerA)
            P <- lowerA + oben2/(1+oben)
            # item x nodes
            Q <- 1 - P
            
            xi <- respm[ep,]
            ximi <- 1-xi
            pxi <- P * xi
            qxi <- Q * ximi
            li  <- pxi + qxi
            
            Li <- apply(li,2,function(x)prod(x,na.rm=TRUE)) 
            EAP <- sum(Li * weimat[ep,] * nodmat[ep,])/sum((Li*weimat[ep,]))
            
            EAP_v <- sum(Li * weimat[ep,] * (nodmat[ep,]-EAP)^2)/sum((Li*weimat[ep,]))
            
            c(EAP,EAP_v)
            })


### draw pvs?!


if(!is.null(npv))
  {
  if(approx)  
    {
    
    pvs <- sapply(1:nrow(respm), function(gretel)
              {
              rnorm(npv,FPL_eap[1,gretel],sqrt(FPL_eap[2,gretel])) 
              })

    } else 
    {
      
    # metropolitan-hastings-algorithm  ########
    #################################################
      jederdritte <- (1:(npv*3))[1:(npv*3) %% 3 == 0]
      
      pvs <- sapply(1:nrow(respm), function(gr)
          {
            
          theta <- FPL_eap[1,gr] # eap estimate
          # 20 for burnin
          # take each 3rd pv
          PVvec <- vector(length=npv*3+20,mode="numeric")
          
           lauf <- 1
    #       zaehl <- 1
          while(lauf <= length(PVvec))
            {
    
              P <- lowerA + (upperA - lowerA) * exp(xi*slopes*(theta - thres[-1,]))/(1+exp(slopes*(theta - thres[-1,])))
    
              Post <- prod(P) * dnorm(theta)
              
              proposed <- rnorm(1,theta,FPL_eap[2,gr])
              
              P1 <-lowerA + (upperA - lowerA) * exp(xi*slopes*(proposed - thres[-1,]))/(1+exp(slopes*(proposed - thres[-1,])))
      
              Post1 <- prod(P1) * dnorm(proposed)
              
              Pmove <- Post1/Post
              
              if(runif(1) <= Pmove)
                {
                  theta <- proposed
                  PVvec[lauf] <- theta
                  lauf <- lauf + 1
                  ##cat(lauf,"\r\r")
                }
              ##zaehl <- zaehl + 1  
            }
          
          PVvec[-(1:20)][jederdritte] 
          })
      
      
    }
    
    
  }
  
colnames(t(FPL_eap)) <- c("EAP","EAP_var")
  
erglist <- list(FPL_eap=FPL_eap,pvs=pvs)
  
 return(erglist) 
  
}














