# Load library
library(quadprog)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter8 e.g.73
# 二次元サポートベクトルマシン
svm_1 <- function(X, y, C) {
  eps <- 0.0001
  n <- nrow(X)
  meq <- 1
  Dmat <- matrix(nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      Dmat[i, j] <- sum(X[i, ] * X[j, ]) * y[i] * y[j]
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
  return(list(beta = beta, beta_0 = beta_0))
}

a <- rnorm(1)
b <- rnorm(1)
n <- 100
X <- matrix(rnorm(n * 2), ncol = 2, nrow = n)
y <- sign(a * X[, 1] + b * X[, 2] + 0.1 * rnorm(n))
plot(-3:3, -3:3,
     xlab = "第1成分", ylab = "第2成分",
     type = "n")
for (i in 1:n) {
  if (y[i] == -1) {
    points(X[i, 1], X[i, 2], col = "red")
  } else {
    points(X[i, 1], X[i, 2], col = "blue")
  }
}
qq <- svm_1(X, y, 10)
abline(-qq$beta_0 / qq$beta[2], -qq$beta[1] / qq$beta[2])