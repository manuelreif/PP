# ------------------------- testing 2>>>


set.seed(1523)
# intercepts
diffpar <- seq(-3,3,length=12)
la     <- round(runif(12,0,0.25),2)
ua     <- round(runif(12,0.8,1),2)
# slope parameters
sl     <- round(runif(12,0.5,1.5),2)

# antwortmatrix (für neue MLE routine)
awm <- matrix(sample(0:1,10*12,replace=TRUE),ncol=12)
awm2 <- awm
awm2[3,2] <- 2

diffparM <- rbind(0,diffpar)
diffparM2 <- diffparM
diffparM2[2,4] <- NA

b1 <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "wle")
b2 <- PP_4pl(respm = awm,thres = diffparM, slopes = sl,type = "wle")

b3 <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "mle")
b4 <- PP_4pl(respm = awm,thres = diffparM, slopes = sl,type = "mle")

#t
test_that("Output = the same on 1,2,3,4pl - with vector or matrix input",{
  expect_equal(b1[[1]], b2[[1]])
  expect_equal(b3[[1]], b4[[1]])
})



# ------------------------- testing 3>>>

#t
test_that("errors - warnings misspelling and length #1",{
  expect_error(PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "aaa"))
  expect_error(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl))
  expect_error(PP_4pl(respm = awm[,-1],thres = diffpar, slopes = sl)) 
  expect_error(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1])) 
  expect_error(PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la[-1])) 
  expect_error(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1])) 
  expect_error(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1],lowerA = la[-1]))
  expect_error(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1],lowerA = la[-1]))
  expect_error(PP_4pl(respm = awm2,thres = diffpar, slopes = sl))
  expect_error(PP_4pl(respm = awm2,thres = diffparM2, slopes = sl))
})



















