# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.63
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

poly <- function(x, y, z = x) {
  n <- length(x)
  m <- length(z)
  X <- cbind(rep(1, n), x, x ^ 2, x ^ 3)
  yy <- array(dim = n)
  beta_hat <- array(dim = 4)
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  X <- cbind(rep(1, m), z, z ^ 2, z ^ 3)
  yy <- X %*% beta_hat
  return(yy)
}

n <- 30
x <- runif(n) * 2 * pi - pi
y <- sin(x) + rnorm(n)
plot(x, y)
y_1 <- 0
y_2 <- 0
for (k in 1:10) {
  y_1 <- poly(x, y - y_2)
  y_2 <- local(x, y - y_1)
}
z <- seq(-2, 2, 0.1)
par(mfrow = c(1, 2))
plot(z, poly(x, y_1, z), type = "l",
     xlab = "x", ylab = "f(x)",
     main = "多項式回帰（3次）", col = "blue")
plot(z, local(x, y_2, z), type = "l",
     xlab = "x", ylab = "f(x)",
     main = "局所線形回帰", col = "blue")
