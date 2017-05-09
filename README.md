PP
==

[![](http://www.r-pkg.org/badges/version/PP)](http://www.r-pkg.org/pkg/PP)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/PP)](http://www.r-pkg.org/pkg/PP)

### Version 0.6.0-05 is on github, and will be soon on cran!


This R package provides Person Parameter estimation for the 1,2,3,4PL model and the generalized partial credit model. This package will soon be uploaded to cran!


To install this package from github, install [devtools](https://github.com/hadley/devtools) first.

```R
library(devtools)
install_github("PP", "manuelreif", ref="master")
```

### What's NEW?

* [Jan Steinfeld](https://github.com/jansteinfeld) is now on board! 
* Jan programmed the great `Pfit()` function, to estimate a bunch of **fit indices**!
*  Use the brandnew `PPass()` function, to estimate Person Parameters and fit indices in one step!
* We added a real-world dataset, which contains responses to a adaptive intelligence test. Load the data and start trying the functions of PP.
* You can now fit your model with the great `eRm` package, and put the object into the `PPass()` function to estimate person parameters and person fit indices.




### Example

Here is a small example of the new `PPass()` function.

```R
> data(pp_amt)
> 
> d <- pp_amt$daten_amt
> 
> rd_res <- PPass(respdf = d, 
+                 items = 8:ncol(d),
+                 mod="1PL",
+                 thres = pp_amt$betas[,2], 
+                 fitindices = "lz")
Estimating:  1pl model ... 
type = wle 
Estimation finished!
> 
> head(rd_res)
   estimate        SE        lz   lz_unst
1 1.4098582 0.4177663 -0.631025 -0.613445
2 0.3515598 0.3728682  1.229631 -0.627897
3 1.0745005 0.3741312  1.097723 -0.627596
4 0.3204690 0.6900113  0.981567 -0.494311
5 1.3879169 0.4643963 -0.203400 -0.600039
6 0.9457360 0.4184997  0.573956 -0.615722
```

