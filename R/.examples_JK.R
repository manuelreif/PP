### 4pl jackknife

### data creation ##########

set.seed(1622)
# intercepts
diffpar <- seq(-3,3,length=12)
# slope parameters
sl     <- round(runif(12,0.5,1.5),2)
la     <- round(runif(12,0,0.25),2)
ua     <- round(runif(12,0.8,1),2)

# response matrix
awm <- matrix(sample(0:1,10*12,replace=TRUE),ncol=12)



## 1PL model ##### 

# MLE estimation
res1plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "mle")
# WLE estimation
res1plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "wle")
# MAP estimation
res1plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "map")


## centering method = mean
res_jk1 <- JKpp(res1plmle)
res_jk2 <- JKpp(res1plwle)
res_jk3 <- JKpp(res1plmap)

## centering method = median
res_jk1a <- JKpp(res1plmle,cmeth = "median")
res_jk2a <- JKpp(res1plwle,cmeth = "median")
res_jk3a <- JKpp(res1plmap,cmeth = "median")

## centering method = AMT
res_jk1b <- JKpp(res1plmle,cmeth = "AMT")
res_jk2b <- JKpp(res1plwle,cmeth = "AMT")
res_jk3b <- JKpp(res1plmap,cmeth = "AMT")




## 2PL model ##### 

# MLE estimation
res2plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "mle")
# WLE estimation
res2plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "wle")
# MAP estimation
res2plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "map")



res_jk4 <- JKpp(res2plmle)
res_jk5 <- JKpp(res2plwle)
res_jk6 <- JKpp(res2plmap)



### gpcm jackknife



# some threshold parameters
THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
# slopes
sl     <- c(0.5,1,1.5,1.1,1,0.98)
awmatrix <- matrix(c(1,0,2,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)



## PCM model ##### 

# MLE estimation
respcmlmle <- PP_gpcm(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "mle")
# WLE estimation
respcmwle <- PP_gpcm(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "wle")
# MAP estimation
respcmmap <- PP_gpcm(respm = awmatrix,thres = THRES, slopes = rep(1,ncol(THRES)),type = "map")


res_jk7 <- JKpp(respcmlmle)
res_jk8 <- JKpp(respcmwle)
res_jk9 <- JKpp(respcmmap)



