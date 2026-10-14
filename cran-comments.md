## Submission

PP 1.0.0 is a rewrite of the numerical core, validated against independent
reference implementations. Some results change compared to 0.6.4-1 (mainly in
edge cases); NEWS.md lists them together with the few breaking changes.

## Maintainer change

The maintainer changes from Jan Steinfeld <jan.d.steinfeld@gmail.com> back to
Manuel Reif <manuel.reif@gmail.com>, the original author and maintainer up to
0.6.4. Jan Steinfeld remains an author and will confirm the change by email.

## Test environments

* local: Ubuntu 22.04, R 4.6.1, `R CMD check --as-cran`
* R-hub v2 (GitHub Actions), all with status OK:
  * R-devel: linux (Ubuntu 24.04), windows (Windows Server 2022),
    macos (x86_64), macos-arm64, ubuntu-clang, gcc16, clang22, nold
  * R 4.6.1: ubuntu-release, lto
  * additional checks: clang-asan, clang-ubsan, gcc-asan, m1-san, valgrind
    (no sanitizer or valgrind errors), nosuggests
* win-builder: R-devel (2026-10-13 r90668 ucrt) and R 4.6.1 (ucrt), 1 NOTE
  each (see below)

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer (see above).
* Possibly misspelled words in DESCRIPTION: Schuster, Snijders and Wainer are
  author names of the cited papers; lz and infit are names of person fit
  statistics. All are spelled correctly.

## Reverse dependencies

There is one reverse dependency, mstDIF 0.1.8 (Imports). It passes
`R CMD check --run-donttest` with PP 1.0.0 with status OK, as with 0.6.4-1.
mstDIF calls `PP_4pl()` and reads `$resPP$resPP`, which 1.0.0 keeps; its WLE
estimates differ from those of 0.6.4-1 by at most 4.4e-5.
