generateX1 <- function(n1, sigma1, p1) {
  F1 <- runif(n1, min = -3, max = 3)
  X1.weight <- runif(n1)
  
  X1.Norm.ind.matr <- matrix(unlist(lapply(p1, function(p) X1.weight <= p)), nrow = length(X1.weight))
  X1.F1.ind.mart <- !X1.Norm.ind.matr
  
  X1.F1 <- F1*X1.F1.ind.mart
  
  X1.Norm <- rnorm(n1, mean = 0, sd = sigma1)
  X1 <- t(X1.Norm*X1.Norm.ind.matr + X1.F1)
  
  return(X1)
}

generateX2n <- function(n2, sigma2, p2, weights) {
  F2 <- runif(n2, min = -3, max = 3)
  X2.weight <- runif(n2)
  
  X2.Norm.ind.matr <- matrix(unlist(lapply(p2, function(p) X2.weight <= p)), nrow = length(X2.weight))
  X2.F2.ind.mart <- !X2.Norm.ind.matr
  
  X2.F2 <- F2*X2.F2.ind.mart
  
  # Генерируем матрицу, в строках которой будут выбороки из
  # нормального распределения с sigma2[ind], где ind номер строки матрицы
  X2.norm.matr <- matrix(rnorm(n2*length(sigma2), mean = 0, sd = sigma2), nrow=length(sigma2))
  
  # Разделяем матрицу выборок на список выборок
  X2.norm.list <- split(X2.norm.matr, 1:length(sigma2))
  
  # Для каждой конкретной выборки X2.norm создаем  X2
  X2n <- lapply(X2.norm.list, function(X2.norm) t(X2.Norm.ind.matr*X2.norm + X2.F2))
  
  return(X2n)
}