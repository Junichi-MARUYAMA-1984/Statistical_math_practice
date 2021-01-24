# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.61
D <- function(t) {
  return(max(0.75 * (1 - t ^ 2), 0))
}

K <- function(x, y, lambda) {
  return(D(abs(x - y) / lambda))
}

local <- function(x, y, z = x) {
  X <- cbind(rep(1, n), x)
  yy <- NULL
  beta_hat <- array(dim = 2)
  for (u in z) {
    w <- array(dim = n)
    for (i in 1:n) {
      w[i] <- K(x[i], u, lambda = 1)
    }
    W <- diag(w)
    beta_hat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y
    yy <- c(yy, beta_hat[1] + beta_hat[2] * u)
  }
  return(yy)
}

n <- 30
x <- runif(n) * 2 * pi - pi
y <- sin(x) + rnorm(n)
plot(x, y)
m <- 200
U <- seq(-pi, pi, pi / m)
V <- local(x, y, U)
lines(U, V, col = "red", type = "l")
title("局所線形回帰 (p = 1, N = 30)")