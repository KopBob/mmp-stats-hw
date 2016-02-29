source("./generator.R")

pvpow <- function(cl, n1, n2, p1, p2, sigma1, sigma2) {
  N <- length(sigma2)
  M <- length(p1)

  print("Генерируем выборки")
  X1 <- generateX1(n1, sigma1, p1)
  X2 <- generateX2(n2, sigma2, p2)
  
  X1.X2 <- cbind(X1, X2)
  
  PV_L <- parApply(cl, X1.X2, 1, function(pair) var.test(pair[[1]], pair[[2]])$p.value)
  
  PV_M <- matrix(PV_L, nrow = N,  byrow=TRUE)
  PW_M <- PV_M < 0.05
  
  return(list(PV_M, PW_M))
}