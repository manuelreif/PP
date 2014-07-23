
conv_diag <- function(respm,thres,slopes,lowerA,upperA,theta_start, 
                      type, maxsteps, exac, mu, sigma2,modest,maxsc=NULL)
{
  

  ## das gehoert noch in PPall eingebaut mit versteckter control
  
  

verb_erg <- vector(length=maxsteps,mode="list")  
  
  
if(modest %in% c("2pl","3pl","4pl","3pl_upperA"))
  {
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
        
    
    #resPP <- NR_4PL(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start, wm=type,maxsteps,exac,mu,sigma2)
    
    if(type=="wle")
    {
      
      for(lauf in 1:maxsteps)
      {
        verb_erg[[lauf]] <-  PP:::L4pl_wle(awm=respm,DELTA=thres,ALPHA=slopes,
                                      CS=lowerA,DS=upperA,THETA= theta_start)
        
        # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
        diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,6]),5]
        
        if(all(abs(diffs1)< exac)  | lauf == maxsteps)
          {
          break   
          }
        
        theta_start <- verb_erg[[lauf]][,6]
      }
    } else if(type=="mle")
        {
          for(lauf in 1:maxsteps)
            {
              verb_erg[[lauf]] <-  PP:::L4pl(awm=respm,DELTA=thres,ALPHA=slopes,
                                        CS=lowerA,DS=upperA,THETA= theta_start,
                                        map=FALSE,mu,sigma2)
              
              # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
              diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
              
              if(all(abs(diffs1)< exac) | lauf == maxsteps)
              {
                break   
              }
              
              theta_start <- verb_erg[[lauf]][,4]
            }
         }  else if(type == "map")
             {
            
              for(lauf in 1:maxsteps)
                {
                  verb_erg[[lauf]] <-  PP:::L4pl(awm=respm,DELTA=thres,ALPHA=slopes,
                                            CS=lowerA,DS=upperA,THETA= theta_start,
                                            map=TRUE,mu,sigma2)
                  
                  # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
                  diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
                  
                  if(all(abs(diffs1)< exac) | lauf == maxsteps)
                  {
                    break   
                  }
                  
                  theta_start <- verb_erg[[lauf]][,4]
                } 
            
            }
    

  } else if(modest == "GPCM")
      {
        
        #resPP <- NR_GPCM(respm,thres,slopes,theta_start,type,maxsteps,exac,mu,sigma2)  
        
        if(type=="wle")
        {
          
          for(lauf in 1:maxsteps)
          {
            verb_erg[[lauf]] <-  PP:::L12gpcm_wle(awm=respm,DELTA=thres,ALPHA=slopes,
                                                  THETA= theta_start)
            
            diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,6]),5]
            
            if(all(abs(diffs1)< exac)  | lauf == maxsteps)
            {
              break   
            }
            
            theta_start <- verb_erg[[lauf]][,6]
          }
        } else if(type=="mle")
        {
          for(lauf in 1:maxsteps)
          {
            verb_erg[[lauf]] <-  PP:::L12gpcm(awm=respm,DELTA=thres,ALPHA=slopes,
                                              THETA= theta_start,
                                              map=FALSE,mu,sigma2)
            
            # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
            diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
            
            if(all(abs(diffs1)< exac) | lauf == maxsteps)
            {
              break   
            }
            
            theta_start <- verb_erg[[lauf]][,4]
          }
        }  else if(type == "map")
        {
          
          for(lauf in 1:maxsteps)
          {
            verb_erg[[lauf]] <-  PP:::L12gpcm(awm=respm,DELTA=thres,ALPHA=slopes,
                                              THETA= theta_start,
                                              map=TRUE,mu,sigma2)
            
            # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
            diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
            
            if(all(abs(diffs1)< exac) | lauf == maxsteps)
            {
              break   
            }
            
            theta_start <- verb_erg[[lauf]][,4]
          } 
          
        }
        

        
      } else {
        ## MIXED THING   
        # determine the model per item
          model2est <- ifelse(maxsc > 1,"GPCM","4PL")
          

          #resPP <-  NR_mixed(respm,DELTA = thres,ALPHA = slopes, CS = lowerA, DS = upperA, THETA = theta_start,model=model2est, wm=type,maxsteps,exac,mu,sigma2)
          
          
          if(type=="wle")
          {
            
            for(lauf in 1:maxsteps)
            {
              verb_erg[[lauf]] <-  PP:::Lgpcm4pl_wle(awm=respm,DELTA=thres,ALPHA=slopes,
                                                     CS=lowerA,DS=upperA,THETA= theta_start,
                                                     model=model2est)
              
              # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
              diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,6]),5]
              
              if(all(abs(diffs1)< exac)  | lauf == maxsteps)
              {
                break   
              }
              
              theta_start <- verb_erg[[lauf]][,6]
            }
          } else if(type=="mle")
          {
            for(lauf in 1:maxsteps)
            {
              verb_erg[[lauf]] <-  PP:::Lgpcm4pl_mle(awm=respm,DELTA=thres,ALPHA=slopes,
                                                     CS=lowerA,DS=upperA,THETA= theta_start,
                                                     model=model2est,
                                                     map=FALSE,mu,sigma2)
              
              # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
              diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
              
              if(all(abs(diffs1)< exac) | lauf == maxsteps)
              {
                break   
              }
              
              theta_start <- verb_erg[[lauf]][,4]
            }
          }  else if(type == "map")
          {
            
            for(lauf in 1:maxsteps)
            {
              verb_erg[[lauf]] <-  PP:::Lgpcm4pl_mle(awm=respm,DELTA=thres,ALPHA=slopes,
                                                     CS=lowerA,DS=upperA,THETA= theta_start,
                                                     model=model2est,
                                                     map=TRUE,mu,sigma2)
              
              # wenn theta NA ist, dann wird diese differenz rausgenommen - siehe cpp code
              diffs1 <- verb_erg[[lauf]][!is.na(verb_erg[[lauf]][,3]),3]
              
              if(all(abs(diffs1)< exac) | lauf == maxsteps)
              {
                break   
              }
              
              theta_start <- verb_erg[[lauf]][,4]
            } 
            
          } 

          
        }  
  
  

### check whether the difference gets smaller and smaller in each interation step #### 

if(type=="wle")
  {
  # raw results  
  ergcov1 <- sapply(verb_erg, function(x)x[,5])
  # TRUE if step i is smaller than i - 1
  ergcov2 <-matrix(TRUE,nrow(ergcov1),ncol(ergcov1)-1)
  
  for(i in 2:10)
    {
      
    ergcov2[,i-1] <- abs(ergcov1[,i]) <= abs(ergcov1[,i-1])
      
    }
    
    
  } else 
    {
      
      # raw results  
      ergcov1 <- sapply(verb_erg, function(x)x[,3])
      # TRUE if step i is smaller than i - 1
      ergcov2 <- matrix(TRUE,nrow(ergcov1),ncol(ergcov1)-1)
      
      for(i in 2:10)
        {
          
          ergcov2[,i-1] <- abs(ergcov1[,i]) <= abs(ergcov1[,i-1])
          
        }

    }





retlist <- list(verb_erg=verb_erg,ergcov1=ergcov1,ergcov2=ergcov2)

  
  
return(retlist)  
  
}


















