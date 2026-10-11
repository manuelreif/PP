################# Plausible values #############################################################

### 2PL model ######

set.seed(1522)
# difficulties and slopes
diffpar <- seq(-3, 3, length = 12)
sl      <- round(runif(12, 0.5, 1.5), 2)

# response matrix
awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 12), upperA = rep(1, 12),
               theta = rnorm(10))

# plausible values need a prior: EAP (or MAP) estimation, here with the default N(0, 1)
res2pl <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, type = "eap")

# 10 plausible values per person, normal approximation of the posterior
res_pv <- PV(res2pl)
summary(res_pv)

# 200 draws from the posterior itself; their means approximate the EAP
res_pv2 <- PV(res2pl, npv = 200, approx = FALSE)
cbind(pv_mean = rowMeans(res_pv2$pvdraws),
      eap = res2pl$estimates$estimate)


### GPCM model with person-specific priors ######

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
sl     <- c(0.5,1,1.5,1.1,1,0.98)
awmatrix <- matrix(c(1,0,2,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)

resgpcm <- PP_gpcm(respm = awmatrix, thres = THRES, slopes = sl, type = "eap",
                   mu = c(0, 0, 1, 1, 1), sigma2 = 0.5)
PV(resgpcm, npv = 5, approx = FALSE)


### GPCM and 4PL items mixed ######

THRESx <- THRES
THRESx[2,1:3] <- NA
la     <- c(0.02,0.1,0,0,0,0)
ua     <- c(0.97,0.91,1,1,1,1)
awmatrix <- matrix(c(1,0,1,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1
                     ,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)

respcm <- PPall(respm = awmatrix, thres = THRESx, slopes = sl, lowerA = la, upperA = ua,
                type = "eap", model2est = findmodel(THRESx))
res_mixedpv <- PV(respcm, npv = 200, approx = FALSE)

# quantiles of the draws per person
apply(res_mixedpv$pvdraws, 1, quantile)
