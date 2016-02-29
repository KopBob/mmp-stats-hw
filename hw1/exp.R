source("./pvpow.R")

library(parallel)


experiment <- function(n1, n2, p1, p2, sigma1, sigma2, iterations = 1) {
  cl <- makeCluster(detectCores())
  
  N <- length(sigma2)
  M <- length(p1)
  
  PV_M  <- rep(0, N * M)
  PW_M  <- rep(0, N * M)
  
  for(i in 1:iterations) {
    ans <- pvpow(cl, n1, n2, p1, p2, sigma1, sigma2)
    PV_M <- PV_M + ans[[1]]
    PW_M <- PW_M + ans[[2]]
  }
  
  PV_M <- PV_M/iterations
  PW_M <- PW_M/iterations
  
  return(list(PV_M, PW_M))
}