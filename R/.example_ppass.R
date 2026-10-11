## ========== data with item parameters

set.seed(1337)
# difficulties and slopes of 15 items
diffpar <- seq(-3, 3, length = 15)
sl <- round(runif(15, 0.5, 1.5), 2)
# responses of 200 persons, plus an id and a group variable
awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 15), upperA = rep(1, 15),
               theta = rnorm(200))
dat <- data.frame(id = 1:200, group = rep(c("a", "b"), 100), awm)

# 2PL, WLE estimates and all person fit indices; id and group are kept
res <- PPass(dat, items = 3:17, mod = "2PL", thres = diffpar, slopes = sl)
res
summary(res)

# a plain data.frame for further use
head(as.data.frame(res))

# jackknife directly from the result
jk <- JKpp(res)


## ========== real data (adaptive test, 1PL)

data(pp_amt)
d <- pp_amt$daten_amt
rd_res <- PPass(d, items = 8:ncol(d), mod = "1PL", thres = pp_amt$betas[, 2],
                fitindices = "lz")
head(rd_res)


## ========== models fitted with eRm

if (requireNamespace("eRm", quietly = TRUE)) {

  # Rasch model
  my_rm <- eRm::RM(eRm::raschdat1)
  res_rm <- PPass(my_rm)
  summary(res_rm)

  # the maximum likelihood estimates equal eRm::person.parameter()
  res_mle <- PPass(my_rm, type = "mle", fitindices = "lzstar")

  # partial credit model
  set.seed(2751)
  thres <- rbind(0, matrix(c(-2, -1, 0.5, -1.2, 0.3, NA, -0.5, 1, NA, 0, 1.5, 2), 3))
  simdat <- sim_gpcm(thres = thres, alpha = rep(1, 4), theta = rnorm(300))
  res_pcm <- PPass(eRm::PCM(simdat))

  # linear partial credit model with two time points and two groups
  simdat2 <- sim_gpcm(thres = thres, alpha = rep(1, 4), theta = rnorm(300, 0.5))
  my_lpcm <- eRm::LPCM(cbind(simdat, simdat2), mpoints = 2, groupvec = rep(1:2, 150))
  res_lpcm <- PPass(my_lpcm)
  head(res_lpcm)

  # plausible values need an EAP or MAP estimate
  pv <- PV(PPass(my_rm, type = "map"))
}
