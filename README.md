PP
==

This R package provides Person Parameter estimation for the 1,2,3,4PL model and the generalized partial credit model. This package will soon be uploaded to cran, and will replace the old PP package which does similar computation, but with far less speed. This new version of the PP package makes use of the great Rcpp package to accelerate computations.


To install this package from github, install [devtools](https://github.com/hadley/devtools) first.

```R
library(devtools)
install_github("PP", "manuelreif", ref="master")
```


TO-DOs for the next release version:

* add print and summary routines
* add robust person parameter estimation
* (perhaps) add some person fit indices


