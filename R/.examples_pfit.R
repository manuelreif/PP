################# Pfit ###################################################

### dichotomous items ##########

set.seed(1337)
diffpar <- seq(-3, 3, length = 15)
sl      <- round(runif(15, 0.5, 1.5), 2)
la      <- round(runif(15, 0, 0.25), 2)
awm     <- sim_4pl(beta = diffpar, alpha = sl, lowerA = la, upperA = rep(1, 15),
                   theta = rnorm(100))

# 3PL, WLE
res3plwle <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, lowerA = la, type = "wle")

# the responses are taken from the estimation object
fit3pl <- Pfit(pp = res3plwle, fitindices = c("lz", "lzstar", "infit", "outfit"))
head(fit3pl$lzstar)
summary(fit3pl)

# with missing responses and jackknife standard errors
awm_na <- awm
awm_na[sample(length(awm), 300)] <- NA
res3plna <- PP_4pl(respm = awm_na, thres = diffpar, slopes = sl, lowerA = la, type = "wle")
head(Pfit(pp = res3plna, fitindices = c("lz", "infit"), SE = TRUE)$lz)

### GPCM ##########

thres <- rbind(0, c(-1, -0.5, 0), c(0.5, 0.8, 1), c(1.5, 2, 2.5))
slg   <- c(0.8, 1.2, 1)
respg <- sim_gpcm(thres = thres, alpha = slg, theta = rnorm(50))
resgwle <- PP_gpcm(respm = respg, thres = thres, slopes = slg, type = "wle")
summary(Pfit(pp = resgwle))

### GPCM and 4PL mixed ##########

THRESx <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
THRESx[2, 1:3] <- NA
slm <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
lam <- c(0.02, 0.1, 0, 0, 0, 0)
uam <- c(0.97, 0.91, 1, 1, 1, 1)
awmatrix <- matrix(c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1,
                     2, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                     1, 2, 2, 1, 1, 1, 1, 0, 0, 1), byrow = TRUE, nrow = 5)
resmixed <- PPall(respm = awmatrix, thres = THRESx, slopes = slm, lowerA = lam,
                  upperA = uam, type = "wle", model2est = findmodel(THRESx))
Pfit(pp = resmixed, fitindices = c("lz", "lzstar"))
