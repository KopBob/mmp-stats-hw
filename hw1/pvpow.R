source("./generator.R")

pvpow <- function(n1, n2, p1, p2, sigma1, sigma2) {
  print("Генерируем выборки")
  X1 <- generate.X1(n1, sigma1, p1)
  X2n <- generate.X2n(n2, sigma2, p2)
  
  
  # список выборок из X1
  X1.list <- split(X1, 1:N)
  
  # Список по сигма, списокв по p
  X2n.list <- lapply(X2n, function(X2) split(X2,  1:N))
  
  # Список по сигма, списков пар X1,X2 по p 
  X2n.X1 <- lapply(X2n.list, function(X2.list) split(t(rbind(X1.list, X2.list)), 1:N))
  
  print("Считаем p-value")
  p.value.matr <- lapply(X2n.X1, function(X.sigm) 
    lapply(X.sigm, function(X) var.test(unlist(X[1]), unlist(X[2]))$p.value)
  )
  
  PV_M <- matrix(unlist(p.value.matr), nrow=N, ncol=M)
  PW_M <- PV_M < 0.05
  
  return(list(PV_M, PW_M))
}