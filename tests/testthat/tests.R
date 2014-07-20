# tests #########################
########## 2/3/4PL Model ########
################################################################


# ------------------------- CREATE DATA >>>

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1,2,-1,-0.2),nrow=2)
THRESx <- rbind(0,THRES)
sl     <- c(0.5,1,1.5,1.1)

awmatrix <- matrix(c(1,0,1,0,1,1,1,0),nrow=2,byrow=TRUE)
awmatrix <- rbind(awmatrix,c(1,1,1,1),c(0,0,0,0),c(1,0,1,0),c(1,0,1,0),c(1,1,0,0))

ua     <- c(0.98,0.85,0.9,0.95)
la     <- c(0,0.05,0.12,0.001)


estmod <- rep(c("mle","wle","map"),4)
LA <- vector(mode="list",length=length(estmod))
UA <- vector(mode="list",length=length(estmod))
for(i in c(4,5,6,10,11,12))
  {
    LA[[i]] <- la      
  }

for(i in c(7,8,9,10,11,12))
  {
    UA[[i]] <- ua
  }
    

res234pl_dup1 <- vector(mode="list",length=length(estmod))
res234pl_dup2 <- vector(mode="list",length=length(estmod))


for(a in 1:length(estmod))
  {
    res234pl_dup1[[a]]  <- PPall(awmatrix,THRESx,slopes = sl,theta_start=rep(0,nrow(awmatrix)),type = estmod[[a]],ctrl = list(killdupli=TRUE),upperA = UA[[a]],lowerA = LA[[a]])
  
    res234pl_dup2[[a]] <- PPall(awmatrix,THRESx,slopes = sl,theta_start=rep(0,nrow(awmatrix)),type = estmod[[a]],ctrl = list(killdupli=FALSE),upperA = UA[[a]],lowerA = LA[[a]])
    
  }

# ------------------------- testing >>>

test_that("Output = the same - with or without removing duplicates"{

for(te in 1:length(estmod))
{
expect_that(res234pl_dup1[[te]],equals(res234pl_dup1[[te]]))  
}

})












