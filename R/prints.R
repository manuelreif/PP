
#'@export
#'@rdname PPall
#'@param x an object of class gpcm4pl which is the result of using the \code{PPall()} function
#'@param ... just some points.
#'@method print gpcm4pl
print.gpcm4pl <-
  function(x, ...)
  {
  print(apply(x$resPP$resPP,2,function(qq) round(qq,4)))
  }

    

#'@export
#'@rdname PPall
#'@param object an object of class gpcm4pl which is the result of using the \code{PPall()} function
#'@method summary gpcm4pl
summary.gpcm4pl <-
  function(object, ...)
  {
  cat("PP Version: ",as.character(attr(object$call,"version")),"\n")
  cat("\n Call:",deparse(object$call),"\n- job started @",attr(object$call,"date"),"\n\n\n")  
  
  cat("Number of iterations:",  object$resPP$nsteps,"\n")
  
  cat("-------------------------------------\n")
  
  apply(object$resPP$resPP,2,function(x) round(x,4))
  
  }











