PP
==

This R package provides Person Parameter estimation for the 1,2,3,4PL model and the generalized partial credit model. This package will soon be uploaded to cran, and will replace the old PP package which does similar computation, but with far less speed. This new version of the PP package makes use of the great [Rcpp](https://github.com/RcppCore/Rcpp) package to accelerate computations.


To install this package from github, install [devtools](https://github.com/hadley/devtools) first.

```R
library(devtools)
install_github("PP", "manuelreif", ref="master")
```


TO-DOs for the next release version:

* **fit indices**: these will be built in by [Jan Steinfeld](https://github.com/jansteinfeld)
* add robust person parameter estimation [in progress: jk = almost done, jk_AMT = almost done, huber estimates = done for 4pl - in a few days for GPCM and PPall]
* EAP [done for 4PL & GPCM + next: PPall]
* Plausible values [done for 4PL & GPCM + next: PPall]
* add print and summary routines [done] - some more would be nice


