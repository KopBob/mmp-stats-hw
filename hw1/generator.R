generateX1 <- function(n1, sigma1, p1) {
  F1 <- runif(n1, min = -3, max = 3)
  X1.weight <- runif(n1)
  
  X1.Norm.ind.matr <- t(vapply(p1, function(p) X1.weight <= p, FUN.VALUE = logical(n1)))
  X1.F1.ind.mart <- !X1.Norm.ind.matr
  
  X1.F1 <- F1*X1.F1.ind.mart
  
  X1.Norm <- rnorm(n1, mean = 0, sd = sigma1)
  X1.matr <- X1.Norm*X1.Norm.ind.matr + X1.F1
  
  X1 <- split(X1.matr, 1:length(p1))
  
  return(X1)
}

generateX2 <- function(n2, sigma2, p2, weights) {
  F2 <- runif(n2, min = -3, max = 3)
  X2.weight <- runif(n2)
  
  X2.Norm.ind.matr <- t(vapply(p2, function(p) X2.weight <= p, FUN.VALUE = numeric(n2)))
  X2.F2.ind.mart <- !X2.Norm.ind.matr
  
  X2.F2 <- F2*X2.F2.ind.mart
  
  X2.norm.matr <- matrix(rnorm(n2*length(sigma2), mean = 0, sd = sigma2), nrow=length(sigma2))
  X2.norm.list <- split(X2.norm.matr, 1:length(sigma2))
  
  mixX2Func <- function(X2.sigma.norm) split(X2.Norm.ind.matr*X2.sigma.norm + X2.F2, 1:length(p2))
  
  X2 <- unlist(lapply(X2.norm.list, mixX2Func), recursive=FALSE)
  
  return(X2)
}