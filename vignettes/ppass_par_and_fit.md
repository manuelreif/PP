<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Getting started with Personfit in PP}
-->
# Analyzing a simulated data set



```r
library(PP)
library(data.table)
```

## First example


```r
set.seed(1526)

# intercepts
diffpar <- seq(-3.6,3,length=15)
# slope parameters
sl     <- round(runif(15,0.5,1.5),2)
la     <- round(runif(15,0,0.25),2)
ua     <- round(runif(15,0.8,1),2)


awm <- PP::sim_4pl(beta = diffpar, alpha = sl,
                   lowerA = la, upperA = ua,
                   theta = rnorm(200))

awm <- as.data.frame(awm)
```

### Estimating and using person parameters 


```r
out <- PPass(respdf = awm,thres = diffpar,
             items="all", mod=c("1PL"), 
             fitindices=c("lz","lzstar","infit","outfit"))
```

```
## Estimating:  1pl model ... 
## type = wle 
## Estimation finished!
```

```r
# create a data.table
outdt <- data.table(out)

outdt[, ID := 1:.N]
outdt[, roundest := factor(round(estimate,8))]

# Compute Percentile Ranks with the Person Parameters

outdtu <- unique(outdt[,su := .N, keyby=roundest], by="roundest")
outdtu[,PR :=cumsum(su)/sum(su) * 100]

outdt <- outdt[outdtu[,list(roundest,PR)],,on="roundest"]
setorder(outdt,ID)
outdt[, c("su") := NULL]

# done
outdt
```

```
##         estimate        SE        lz   lz_unst    lzstar    infit
##   1:  0.44208265 0.7122795 -1.123225 -0.566720 -1.131614 1.439156
##   2:  0.94642726 0.7235927 -0.995213 -0.530884 -1.021577 1.532953
##   3: -2.62422293 0.7831444 -2.641876 -0.666216 -3.030419 1.499249
##   4: -0.05351983 0.7073232  0.960996 -0.285822  0.961402 0.632956
##   5: -1.04208264 0.7122795 -2.957368 -0.818149 -2.976676 1.926268
##  ---                                                             
## 196: -0.05351983 0.7073232 -0.845575 -0.537251 -0.846244 1.402821
## 197: -1.04208264 0.7122795  0.710918 -0.315292  0.713448 0.753587
## 198:  0.44208265 0.7122795 -0.893957 -0.535292 -0.900981 1.486505
## 199:  0.44208265 0.7122795  0.252382 -0.378149  0.252183 0.928231
## 200: -0.05351983 0.7073232  1.186818 -0.254394  1.187358 0.521505
##           in_t in_chisq in_df in_pv    outfit      ou_t ou_chisq ou_df
##   1:  1.137559   22.501    14 0.069  1.500033  0.799227   22.501    14
##   2:  1.332657   15.864    14 0.322  1.057589  0.399812   15.864    14
##   3:  1.243558  283.959    14 0.000 18.930614  3.398825  283.959    14
##   4: -0.973855    6.083    14 0.964  0.405564 -0.831833    6.083    14
##   5:  2.039333   76.515    14 0.000  5.100981  2.879117   76.515    14
##  ---                                                                  
## 196:  1.059277   18.261    14 0.195  1.217393  0.522043   18.261    14
## 197: -0.576558    7.120    14 0.930  0.474689 -0.515916    7.120    14
## 198:  1.233420   15.970    14 0.315  1.064664  0.353390   15.970    14
## 199: -0.064128   10.502    14 0.725  0.700112 -0.126848   10.502    14
## 200: -1.392271    5.029    14 0.985  0.335272 -1.019910    5.029    14
##      ou_pv  ID    roundest   PR
##   1: 0.069   1  0.44208265 74.5
##   2: 0.322   2  0.94642726 88.5
##   3: 0.000   3 -2.62422293  3.0
##   4: 0.964   4 -0.05351983 58.5
##   5: 0.000   5 -1.04208264 25.5
##  ---                           
## 196: 0.195 196 -0.05351983 58.5
## 197: 0.930 197 -1.04208264 25.5
## 198: 0.315 198  0.44208265 74.5
## 199: 0.725 199  0.44208265 74.5
## 200: 0.985 200 -0.05351983 58.5
```


### Using person fit statistics 







## Second example


Now there are two groups.


```r
awm <- PP::sim_4pl(beta = diffpar, alpha = sl, lowerA = la, 
                   upperA = ua, theta = c(rnorm(500),rnorm(500,0.5,1.2)))

awm <- as.data.frame(awm)
```

### Estimating and using person parameters 


```r
out <- PPass(respdf = awm, thres = diffpar, 
             items="all", mod=c("1PL"), 
             fitindices= c("lz","lzstar","infit","outfit"))
```

```
## Estimating:  1pl model ... 
## type = wle 
## Estimation finished!
```

```r
outdt <- data.table(out)

outdt[, ID := 1:.N]
outdt[, agegroup := rep(c("young","old"),each=500)]
outdt[, roundest := factor(round(estimate,8))]

outdtu <- unique(outdt[,su := .N, keyby=c("agegroup","roundest")], by=c("agegroup","roundest"))[,PR :=cumsum(su)/sum(su) * 100, by=agegroup]

outdt <- outdt[outdtu[,list(agegroup,roundest,PR)],,on=c("agegroup","roundest")]

setorder(outdt,ID)
outdt[, c("su") := NULL]
outdt
```

```
##          estimate        SE        lz   lz_unst    lzstar    infit
##    1: -0.54648017 0.7073232  0.283532 -0.380108  0.283535 0.914011
##    2:  0.44208265 0.7122795 -1.352493 -0.598149 -1.362246 1.381684
##    3:  0.94642724 0.7235928  0.420406 -0.342313  0.422851 0.844185
##    4:  0.94642724 0.7235928 -0.287404 -0.436598 -0.299363 1.114529
##    5:  1.46851268 0.7447171 -0.665507 -0.460029 -0.718673 1.286378
##   ---                                                             
##  996: -1.54642724 0.7235928 -1.231150 -0.562313 -1.262315 1.607482
##  997: -0.05351983 0.7073232 -3.103790 -0.851537 -3.105801 2.230098
##  998:  2.02421867 0.7831539  0.149894 -0.320502  0.122736 1.009137
##  999: -2.06851268 0.7447171 -0.175639 -0.397172 -0.202467 1.181492
## 1000:  2.02421867 0.7831539 -0.865294 -0.446216 -1.023864 1.290598
##            in_t in_chisq in_df in_pv   outfit      ou_t ou_chisq ou_df
##    1: -0.101851   10.597    14 0.717 0.706459 -0.210120   10.597    14
##    2:  1.018334   44.408    14 0.000 2.960520  1.849372   44.408    14
##    3: -0.305596    9.259    14 0.814 0.617256 -0.085055    9.259    14
##    4:  0.416902   14.840    14 0.389 0.989352  0.334926   14.840    14
##    5:  0.818367   16.648    14 0.275 1.109882  0.526179   16.648    14
##   ---                                                                 
##  996:  1.477556   18.067    14 0.204 1.204439  0.530526   18.067    14
##  997:  2.517298   39.081    14 0.000 2.605391  1.796104   39.081    14
##  998:  0.153158    8.016    14 0.888 0.534419  0.276039    8.016    14
##  999:  0.578201   10.219    14 0.746 0.681256  0.178470   10.219    14
## 1000:  0.813772   22.247    14 0.074 1.483122  0.830121   22.247    14
##       ou_pv   ID agegroup    roundest   PR
##    1: 0.717    1    young -0.54648017 40.6
##    2: 0.000    2    young  0.44208265 77.0
##    3: 0.814    3    young  0.94642724 88.2
##    4: 0.389    4    young  0.94642724 88.2
##    5: 0.275    5    young  1.46851268 95.6
##   ---                                     
##  996: 0.204  996      old -1.54642724 11.8
##  997: 0.000  997      old -0.05351983 50.4
##  998: 0.888  998      old  2.02421867 96.4
##  999: 0.746  999      old -2.06851268  4.8
## 1000: 0.074 1000      old  2.02421867 96.4
```

### Using person fit statistics 






