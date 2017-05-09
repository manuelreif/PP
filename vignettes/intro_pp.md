<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Getting started with PP}
-->
# Getting started with PP

Is the `PP` package the right package for you?


1. You have a data set with item responses from several persons
2. You have item parameter estimates (e.g. difficulty parameters) from these items (perhaps from a former application of these items)
3. and these items follow the 1/2/3/4-PL model or the generalized partial credit model
4. You are interested in estimating each persons **ability** with traditional approaches (mle, wle, map, eap) or
5. drawing **plausible values** for each person e.g. to apply secondary analysis or
6. getting **robust** ability estimates with Jackknife Methods or with weighted likelihood approaches


If you agree to question 1-3 and at least to one of question 4-6, load the package an start estimating person parameters.


Now a simple example.


## 2-PL Model


In this example, we want to gain ability estimates for each person in our sample. Each person answered questions in a test which has been scaled by the 2-PL model. Item parameters (difficulty and slope) are known and are supposed as fixed. What are the next steps?

1. Get the item responses and the item parameters inside the R workspace
2. Check your data and reshape them, so they can be processed by the PP functions
3. Decide which type of estimation you prefer (`?PP_4pl`)
4. Run the estimation


### Get

Getting the data inside the R workspace is quite easy in this case, because we merely load the data set which already comes with the PP package. It contains 60 response sets (60 persons answered to a test of 12 items) - and we have additional information, which we do not take into account for now. We first inspect the data, and in a second step informations about item difficulty and the slope parameters are extracted from the dataset attributes.


```r
library(PP)

data(fourpl_df)

dim(fourpl_df)
```

```
## [1] 60 14
```

```r
head(fourpl_df)
```

```
##        id sex Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10
## 1 LVL0694   w     1     1     1     1     0     0     1     0     0      0
## 2 BBU1225   w     1     1     1     1     1     1     1     1     1      0
## 3 MJN2028   w     1     1     1     1     1     1     1     1     0      0
## 4 TSU0771   m     1     1     1     0     1     1     1     1     1      0
## 5 XDS0698   w     1     1    NA     1     1     1     1     0     0      0
## 6 BOS1292   w     0     0     0     0     0     0     0     0     0      0
##   Item11 Item12
## 1      0      0
## 2      0      0
## 3      0      0
## 4      0      1
## 5      0      0
## 6      0      0
```

```r
# extracting the information
diff_par <- attr(fourpl_df,"diffpar")
slope_par <- attr(fourpl_df,"slopes")
```



### Check and Reshape


Now we explore the item response data regarding full or zero score, missing values etc. . Considering this information, we are able to decide which estimation method we will apply.



```r
# extract items and transform the data.frame to matrix
itmat <- as.matrix(fourpl_df[,-(1:2)])


# are there any full scores?
fullsc <- apply(itmat,1,function(x) (sum(x,na.rm=TRUE)+sum(is.na(x))) == length(x))
any(fullsc)
```

```
## [1] FALSE
```

```r
# are there 0 scores?
nullsc <- apply(itmat,1,function(x) sum(x,na.rm=TRUE) == 0)
any(nullsc)
```

```
## [1] TRUE
```

```r
# are there missing values? how many and where?
nasc <- apply(itmat,1,function(x) sum(is.na(x)))
any(nasc > 0)
```

```
## [1] TRUE
```

```r
#which(nasc)

# is our data dichotomous as expected?
apply(itmat,2,function(x) table(x))
```

```
##   Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10 Item11
## 0     9    17    13    16    21    23    24    28    44     53     53
## 1    51    43    46    44    38    37    35    32    16      7      7
##   Item12
## 0     53
## 1      7
```

```r
# are there any duplicates?
rdup <- duplicated(itmat)
sum(rdup)
```

```
## [1] 8
```



### Decide and Run


We use the `PP_4pl()` function for our estimation. So perhaps you are thinking:"Why are we using a function to fit the 4-PL model, when we acutally have a dataset which stems from a 2-PL model scaled test?!" This is because with the `PP_4pl()` function you can fit the:

1. **1-PL model** (Rasch model) by submitting: the data matrix, item difficulties and **nothing else**, since the 1-PL model is merely a 4-PL model with: any slope = 1, any lower asymptote = 0 and any upper asymptote = 1!
2. **2-PL model** by submitting: the data matrix, item difficulties and slope parameters. Lower and upper asymptotes are automatically set to 0 und 1 respectively.
3. **3-PL model** by submitting anything except the upper asymptote parameters
4. **4-PL model** $\rightarrow$ submit all parameters ...

In this case, difficulty parameters and slopes are available, so we will submit them, and a 2-PL model is fitted **automatically**.

We decide to apply a common maximum likelihood estimation (`type = "mle"`), and do not remove duplicated response patterns (see argument: `ctrl=list()`) from estimation because there are only 8 duplicated patterns. If the data set would have been much larger, duplicated patterns are more likely and therefore choosing to remove these patterns would speed up the estimation process significantly. (Choosing this option or not, does not change the numerical results!)



```r
library(PP)

res1plmle <- PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "mle")
```

```
## Estimating:  2pl model ... 
## type = mle 
## Estimation finished!
```

```r
summary(res1plmle)
```

```
## PP Version:  0.6.0.4 
## 
##  Call: PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "mle") 
## - job started @ Tue May  9 19:55:05 2017 
## 
## Estimation type: mle 
## 
## Number of iterations: 5 
## -------------------------------------
##       estimate     SE
##  [1,]  -0.8555 0.7270
##  [2,]   1.9097 0.9416
##  [3,]   1.1454 0.9211
##  [4,]   1.7883 0.9410
##  [5,]   0.4274 0.8792
##  [6,]     -Inf     NA
##  [7,]   2.8310 0.9618
##  [8,]   1.7414 0.9406
##  [9,]  -2.4328 0.8520
## [10,]  -0.2126 0.7750
## [11,]   1.1454 0.9211
## [12,]   4.0258 1.1822
## [13,]  -1.6514 0.7451
## [14,]  -3.6859 1.2125
## [15,]   1.0795 0.9168
## --------> output truncated <--------
```

Some facts:

1. The function returns the **point estimators** and **standard errors** for each person. Whatever the (valid) input was, the main result is always a matrix with two columns and a number of rows which equals the number of persons (`res1plmle$resPP$resPP`).
2. The function deals with **missing values**. These are treated as if the person has never seen this item.
3. The order of the output estimates = order of the input. So the first estimate belongs to the first response pattern of the input data matrix.



### Edit

In the last step, we add the estimates to the data.frame we extracted the item responses from in the first place.



```r
dafest <- data.frame(fourpl_df, res1plmle$resPP$resPP)

head(dafest,10)
```

```
##         id sex Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9
## 1  LVL0694   w     1     1     1     1     0     0     1     0     0
## 2  BBU1225   w     1     1     1     1     1     1     1     1     1
## 3  MJN2028   w     1     1     1     1     1     1     1     1     0
## 4  TSU0771   m     1     1     1     0     1     1     1     1     1
## 5  XDS0698   w     1     1    NA     1     1     1     1     0     0
## 6  BOS1292   w     0     0     0     0     0     0     0     0     0
## 7  KFF1422   w     1     1     1     1     1     1     1     1     1
## 8  DCQ0198   w     1     1     1     1     0     1     1     1     1
## 9  FTT1492   w     0     0     1     1     0     0     0     0     0
## 10 GCP0645   m     1     1     1     1     1     0     0     1     0
##    Item10 Item11 Item12   estimate        SE
## 1       0      0      0 -0.8554973 0.7270360
## 2       0      0      0  1.9096810 0.9415755
## 3       0      0      0  1.1453588 0.9210920
## 4       0      0      1  1.7882890 0.9409769
## 5       0      0      0  0.4274013 0.8792348
## 6       0      0      0       -Inf        NA
## 7       0      0      1  2.8310343 0.9618330
## 8       0      1      0  1.7413789 0.9405966
## 9       0      0      0 -2.4327540 0.8520338
## 10      0      0      0 -0.2125844 0.7749617
```


One shortcoming of the plain maximum likelihood estimate is the fact, that the **extreme scores** do not lead to valid parameter estimates (`-Inf` and `Inf` are hardly useful for practitioners). One possibility to overcome this issue, is to change the estimation method - for instance `type = wle` performs weighted likelihood estimation, which is on the one hand less biased than the mle estimate, and on the other hand provides reasonable estimates for the extreme scores.


### Rerun



```r
library(PP)

res1plwle <- PP_4pl(respm = itmat,thres = diff_par, slopes = slope_par, type = "wle")
```

```
## Estimating:  2pl model ... 
## type = wle 
## Estimation finished!
```

```r
summary(res1plwle)
```

```
## PP Version:  0.6.0.4 
## 
##  Call: PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "wle") 
## - job started @ Tue May  9 19:55:05 2017 
## 
## Estimation type: wle 
## 
## Number of iterations: 5 
## -------------------------------------
##       estimate     SE
##  [1,]  -0.8788 0.7263
##  [2,]   1.9065 0.9416
##  [3,]   1.0816 0.9169
##  [4,]   1.7814 0.9409
##  [5,]   0.3270 0.8679
##  [6,]  -4.6556 1.7254
##  [7,]   2.7746 0.9583
##  [8,]   1.7322 0.9405
##  [9,]  -2.2872 0.8257
## [10,]  -0.2907 0.7668
## [11,]   1.0816 0.9169
## [12,]   3.7316 1.0975
## [13,]  -1.5967 0.7408
## [14,]  -3.2952 1.0712
## [15,]   1.0089 0.9116
## --------> output truncated <--------
```


So, this was what we were finally looking for.


## Parameters and Person Fit 

For estimating person parameters and examining person fit in one step, use `PPass()` (for ass stands for *assessment*). Using this function has several advantages over the using the other methods consecutively.

1. you ony need 1 Function
2. the data input format now is a `data.frame`
3. the data output format is a new `data.frame` with person fit statistics added



```r
pres <- PPass(fourpl_df, items = 3:14, mod="2PL", thres = diff_par, slopes = slope_par, type = "wle")
```

```
## Estimating:  2pl model ... 
## type = wle 
## Estimation finished!
```







