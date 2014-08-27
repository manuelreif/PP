################# simulate 4PL #############################################################


set.seed(1700)

simdat <- sim_4pl(beta=seq(-4,4,length.out=7), alpha=rep(1,7), lowerA=rep(0,7), upperA=rep(1,7), theta=c(0.231,-1.313,1.772,1.601,1.733,-2.001,0.443,3.111,-4.156))


head(simdat)
