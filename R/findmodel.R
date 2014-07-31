#' Create a model2est vector template
#' 
#' This is a small helper function which creates a model2est vector template for the \code{PPall()} function quick and easily. Modify this template as you like.
#'
#' This function tries to guess the model which was applied to each item by using the matrix of threshold parameters. It only discriminates between gpcm and 4pl model.
#' 
#'@param thres An numeric matrix which contains the threshold parameter for each item.
#'
#'
#'
#'@export
#'
#'@author Manuel Reif
findmodel <- function(thres)
{

if(any(thres[1,] != 0))
  {
   thres <- rbind(0,thres)
  }  
  
maxsc <- apply(thres,2,function(x)(length(x) - sum(is.na(x)))-1)
model2est <- ifelse(maxsc > 1,"GPCM","4PL")

model2est  
}
