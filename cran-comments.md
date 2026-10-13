## Submission

PP 1.0.0 is a rewrite of the numerical core. Every estimate is now checked
against independent reference implementations; several results of 0.6.4-1
were wrong and change in this version. NEWS.md lists all changed results and
the few breaking changes.

## Maintainer change

The maintainer changes from Jan Steinfeld <jan.d.steinfeld@gmail.com> back to
Manuel Reif <manuel.reif@gmail.com>, the original author and maintainer up to
0.6.4. Jan Steinfeld remains an author and will confirm the change by email.

## Test environments

* local: Ubuntu 22.04, R 4.6.1, `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer (see above).
* Possibly misspelled words in DESCRIPTION: Schuster, Snijders and Wainer are
  author names of the cited papers; lz, infit, polytomous and posteriori are
  technical terms; 'eRm' is a package name. All are spelled correctly.

## Reverse dependencies

There is one reverse dependency, mstDIF 0.1.8 (Imports). It passes
`R CMD check --run-donttest` with PP 1.0.0 with status OK, as with 0.6.4-1.
mstDIF calls `PP_4pl()` and reads `$resPP$resPP`, which 1.0.0 keeps; its WLE
estimates differ from those of 0.6.4-1 by at most 4.4e-5.
