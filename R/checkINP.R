checkINP <- function(respm, thres, slopes, theta_start, type)
{
  
# type input
match.arg(type,c("mle","wle","map","eap","robust"))
if(length(type) != 1) stop("Submit a single value as 'type'!\n")

# respm  input
if(!is.matrix(respm)) stop("respm must be a matrix!\n")
if(!is.matrix(thres)) stop("thres must be a matrix!\n")

# thres
allNA  <- any(apply(thres[-1,,drop=FALSE],2,function(x) all(is.na(x))))
thresl <- apply(thres,2,function(x) length(x) - sum(is.na(x)))
if(allNA){stop("Check thres input! Some colums contain only NA.\n")}

# respm
itmax <- apply(respm,2,function(x) max(x))
if(any(itmax > (thresl-1))) stop("Responses in categories which so not exist according to thres!\n")
  
# theta
if(length(theta_start) != nrow(respm)) stop("Invalid length of theta_start!\n")

}
