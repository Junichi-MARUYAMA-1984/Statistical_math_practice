# Load library
library(quadprog)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter8 e.g.77
# 非線形カーネルを用いたサポートベクトルマシン
K_linear <- function(x, y) {
  return(t(x) %*% y)
}

K_poly <- function(x, y) {
  return((1 + t(x) %*% y) ^ 2)
}

svm_2 <- function(X, y, C, K) {
  eps <- 0.0001
  n <- nrow(X)
  meq <- 1
  Dmat <- matrix(nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      Dmat[i, j] <- K(X[i, ], X[j, ]) * y[i] * y[j]
    }
  }
  Dmat <- Dmat + eps * diag(n) # 対角成分に小さな値を加えて、正則にする。
  dvec <- rep(1, n)
  Amat <- matrix(nrow = 2 * n + 1, ncol = n)
  Amat[1, ] <- y
  Amat[2:(n + 1), 1:n] <- -diag(n)
  Amat[(n + 2):(2 * n + 1), 1:n] <- diag(n)
  Amat <- t(Amat) # このパッケージでは、Amatは転置にしたものを指定する。
  bvec <- c(0, rep(-C, n), rep(0, n))
  alpha <- solve.QP(Dmat = Dmat, dvec = dvec, 
                    Amat = Amat, bvec = bvec, meq = 1)$solution
  beta <- drop((alpha * y) %*% X)
  index <- (1:n)[eps < alpha & alpha < C - eps]
  beta_0 <- mean(y[index] - X[index, ] %*% beta)
  return(list(alpha = alpha, beta = beta, beta_0 = beta_0))
}

plot_kernel <- function(K, lty) {
  qq <- svm_2(X, y, 1, K)
  alpha <- qq$alpha
  beta_0 <- qq$beta_0
  f <- function(u, v) {
    x <- c(u, v)
    S <- beta_0
    for (i in 1:n) {
      S <- S + alpha[i] * y[i] * K(X[i, ], x)
    }
    return(S)
  }
  u <- seq(-2, 2, 0.1)
  v <- seq(-2, 2, 0.1)
  w <- array(dim = c(41, 41))
  for (i in 1:41) {
    for (j in 1:41) {
      w[i, j] <- f(u[i], v[j])
    }
  }
  contour(u, v, w, level = 0, add = TRUE, lty = lty)
}

a <- 3
b <- -1
n <- 200
X <- matrix(rnorm(n * 2), ncol = 2, nrow = n)
y <- sign(a * X[, 1] + b * X[, 2] ^ 2 + 0.3 * rnorm(n))
plot(-3:3, -3:3,
     xlab = "X[, 1]", ylab = "X[, 2]",
     type = "n")
for (i in 1:n) {
  if (y[i] == -1) {
    points(X[i, 1], X[i, 2], col = "red")
  } else {
    points(X[i, 1], X[i, 2], col = "blue")
  }
}
plot_kernel(K_linear, 1)
plot_kernel(K_poly, 2)