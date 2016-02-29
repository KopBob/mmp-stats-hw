library(fields)

source("./exp.R")

draw <- function(pv_m, pw_m, x, y) {
  N <- length(x)
  M <- length(y)
  
  grid    <- expand.grid(x=x, y=y)
  x.grid <- matrix(grid$x, nrow=N, ncol=M)
  y.grid <- matrix(grid$y, nrow=N, ncol=M)
  
  par(mfrow=c(1,2))
  image.plot(y.grid, x.grid, pv_m, 
             col = colorRampPalette(c("blue", "cyan", "yellow", "red"))(1024), 
             main="Fisher Test p-values", xlab=expression(p), ylab=expression(sigma))
  
  image.plot(y.grid, x.grid, pw_m, 
             col = colorRampPalette(c("blue", "cyan", "yellow", "red"))(1024), 
             main="Fisher Test power", xlab=expression(p), ylab=expression(sigma))
}


iterations = 2

sigma1 = 1
sigma2 = seq(from = 0.5, to = 2, by = 0.01)
p1 = p2 = seq(from = 0, to = 1, by = 0.01)
n1 = n2 = 50

result <- experiment(n1, n2, p1, p2, sigma1, sigma2, iterations)

PV_M <- result[[1]]
PW_M <- result[[2]]

draw(PV_M, PW_M, sigma2, p1)