### 4pl - plausible values 

### data creation ##########

set.seed(1522)
# intercepts
diffpar <- seq(-3,3,length=12)
# slope parameters
sl     <- round(runif(12,0.5,1.5),2)
la     <- round(runif(12,0,0.25),2)
ua     <- round(runif(12,0.8,1),2)

# response matrix
awm <- matrix(sample(0:1,10*12,replace=TRUE),ncol=12)


# EAP estimation - 2pl model
res2pleap <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "eap")

# draw 10 plausible values
res_pv  <- PV(res2pleap)

# draw 10 plausible values - use a metropolitan hastings algorithm
res_pv2  <- PV(res2pleap,approx = FALSE)


# ------ check the PVs


# -- autocorrelation?
autocor <- function(acv)
  {
  cor(acv[-1],acv[-length(acv)]) 
  }


res_pvac  <- PV(res2pleap,approx = FALSE,npv = 200)

# independent draws - so there cannot be any systematic autocorrelation when approx = TRUE. So this acts as a kind of benchmark for the MH-Alg.
res_pvac2  <- PV(res2pleap,approx = TRUE,npv = 200)

apply(res_pvac,1,autocor)
apply(res_pvac2,1,autocor)

# -- autocorrelation distr?


apply(res_pvac,1,quantile)
apply(res_pvac2,1, quantile)




