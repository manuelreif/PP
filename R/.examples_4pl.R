################# 4PL ###########################################################################


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
res1plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "mle")
# WLE estimation
res1plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "wle")
# MAP estimation
res1plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = rep(1,length(diffpar)),type = "map")


## 2PL model ##### 

# MLE estimation
res2plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "mle")
# WLE estimation
res2plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "wle")
# MAP estimation
res2plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "map")



## 3PL model ##### 

# MLE estimation
res3plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "mle")
# WLE estimation
res3plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "wle")
# MAP estimation
res3plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,type = "map")



## 4PL model ##### 

# MLE estimation
res4plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "mle")
# WLE estimation
res4plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "wle")
# MAP estimation
res4plmap <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la,upperA=ua,type = "map")
