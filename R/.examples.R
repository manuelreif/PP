
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
