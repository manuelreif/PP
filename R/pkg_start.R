
.onAttach <- function(libname, pkgname)
{
  packageStartupMessage(paste(pkgname)," package calling ...\nFollow this project on github: https://github.com/jansteinfeld/PP.git")
}
