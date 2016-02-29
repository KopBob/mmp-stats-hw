library(fields)

source("./pvpow.R")

sigma1 = 1
sigma2 = seq(from = 0.5, to = 2, by = 0.01)
p1 = p2 = seq(from = 0, to = 1, by = 0.01)
n1 = n2 = 50

PV_M  <- rep(0, N * M)
PW_M  <- rep(0, N * M)

exps <- 1

for(i in 1:exps)  # for each row
{
  print(i)
  ans <- pvpow(n1, n2, p1, p2, sigma1, sigma2)
  PV_M <- PV_M + ans[[1]]
  PW_M <- PW_M + ans[[2]]
}

PV_M <- PV_M/exps
PW_M <- PW_M/exps

N <- length(sigma2)
M <- length(p1)


grid    <- expand.grid(x=sigma2, y=p1)
sigma2.matr <- matrix(grid$x, nrow=N, ncol=M)
p.matr <- matrix(grid$y, nrow=N, ncol=M)

image.plot(p.matr, sigma2.matr, PV_M, 
           col = colorRampPalette(c("blue", "cyan", "yellow", "red"))(1024), 
           main="Fisher Test p-values", xlab=expression(p), ylab=expression(sigma))