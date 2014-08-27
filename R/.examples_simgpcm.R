################# simulate GPCM #############################################################


THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
# slopes
sl     <- c(0.5,1,1.5,1.1,1,0.98)

THRESx <- rbind(0,THRES)

THETA  <- rnorm(100)

simdat_gpcm <- sim_gpcm(thres = THRESx,alpha = sl,theta = THETA)

head(simdat_gpcm)
