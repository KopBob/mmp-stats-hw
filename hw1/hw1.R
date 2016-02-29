

source("./exp.R")


library(fields)

draw <- function(m, x, y, title) {
  N <- length(x)
  M <- length(y)
  
  grid    <- expand.grid(x=x, y=y)
  x.grid <- matrix(grid$x, nrow=N, ncol=M)
  y.grid <- matrix(grid$y, nrow=N, ncol=M)
  
  image.plot(y.grid, x.grid, m, 
             col = colorRampPalette(c("blue", "cyan", "yellow", "red"))(1024), 
             main=title, xlab=expression(p), ylab=expression(sigma))
}

# "Fisher Test p-values
# "Fisher Test power"

iterations = 2

sigma1 = 1
sigma2 = seq(from = 0.5, to = 2, by = 0.01)
p1 = p2 = seq(from = 0, to = 1, by = 0.01)
n1 = n2 = 50

result <- experiment(n1, n2, p1, p2, sigma1, sigma2, iterations)

PV_M <- result[[1]]
PW_M <- result[[2]]

draw(PV_M, sigma2, p1, "Fisher Test p-values")
draw(PW_M, sigma2, p1, "Fisher Test power")