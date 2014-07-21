


### data creation ##########

set.seed(1522)
# intercepts
diffpar <- seq(-3,3,length=12)
# slope parameters
sl     <- round(runif(12,0.5,1.5),2)
la     <- round(runif(12,0,0.25),2)
ua     <- round(runif(12,0.8,1),2)

# antwortmatrix (für neue MLE routine)
awm <- matrix(sample(0:1,10*12,replace=TRUE),ncol=12)



## 1PL model ##### 

# MLE estimation
res1plmle <- PPall(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "mle")
# WLE estimation
res1plwle <- PPall(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "wle")
# MAP estimation
res1plmap <- PPall(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "map")

summary(res1plmle)
summary(res1plwle)
summary(res1plmap)

## 2PL model ##### 

# MLE estimation
res2plmle <- PPall(respm = awm,thres = diffpar, slopes = sl,type = "mle")
# WLE estimation
res2plwle <- PPall(respm = awm,thres = diffpar, slopes = sl,type = "wle")
# MAP estimation
res2plmap <- PPall(respm = awm,thres = diffpar, slopes = sl,type = "map")



## 3PL model ##### 

# MLE estimation
res3plmle <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "mle")
# WLE estimation
res3plwle <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "wle")
# MAP estimation
res3plmap <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "map")



## 4PL model ##### 

# MLE estimation
res4plmle <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "mle")
# WLE estimation
res4plwle <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "wle")
# MAP estimation
res4plmap <- PPall(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "map")



################# GPCM ###########################################################################


# some threshold parameters
THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
# slopes
sl     <- c(0.5,1,1.5,1.1,1,0.98)
awmatrix <- matrix(c(1,0,2,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)


## GPCM model ##### 

# MLE estimation
resgpcmlmle <- PPall(respm = awmatrix,thres = THRES, slopes = sl,type = "mle")
# WLE estimation
resgpcmwle <- PPall(respm = awmatrix,thres = THRES, slopes = sl,type = "wle")
# MAP estimation
resgpcmmap <- PPall(respm = awmatrix,thres = THRES, slopes = sl,type = "map")



## PCM model ##### 

# MLE estimation
respcmlmle <- PPall(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "mle")
# WLE estimation
respcmwle <- PPall(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "wle")
# MAP estimation
respcmmap <- PPall(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "map")



################# GPCM  ###########################################################################
## with different number of categories

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
THRES1 <- rbind(THRES,c(NA,NA,NA,NA,1.7,1))

awmatrix1 <- matrix(c(1,0,2,0,1,3,1,0,0,1,3,1,0,0
                      ,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)

# MLE estimation
respcmlmle1 <- PPall(respm = awmatrix1,thres = THRES1, slopes = sl,type = "mle")
# WLE estimation
respcmwle1 <- PPall(respm = awmatrix1,thres = THRES1, slopes = sl,type = "wle")
# MAP estimation
respcmmap1 <- PPall(respm = awmatrix1,thres = THRES1, slopes = sl,type = "map")


################# GPCM  and 4PL mixed #############################################################
## 4PL with GPCM

THRESx <- THRES
THRESx[2,1:3] <- NA

# for the 4PL item the estimated parameters are submitted, 
# for the GPCM items the lower asymptote = 0 
# and the upper asymptote = 1.
la     <- c(0.02,0.1,0,0,0,0)
ua     <- c(0.97,0.91,1,1,1,1)


# MLE estimation
respcmlmle1 <- PPall(respm = awmatrix1,thres = THRESx, 
                     slopes = sl,lowerA = la, upperA=ua,type = "mle")
# WLE estimation
respcmwle1 <- PPall(respm = awmatrix1,thres = THRESx, 
                    slopes = sl,lowerA = la, upperA=ua,type = "wle")
# MAP estimation
respcmmap1 <- PPall(respm = awmatrix1,thres = THRESx, 
                    slopes = sl,lowerA = la, upperA=ua, type = "map")
