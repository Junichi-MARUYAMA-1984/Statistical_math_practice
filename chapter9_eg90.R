# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.90
# 主成分回帰
pca_regression <- function(X, y, m) {
  pr <- prcomp(X)
  Z <- pr$x[, 1:m]
  phi <- pr$rotation[, 1:m]
  theta <- solve(t(Z) %*% Z) %*% t(Z) %*% y
  beta <- phi %*% theta
  return(list(theta = theta, beta = beta))
}

n <- 100
p <- 5
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
for (j in 1:p) {
  X[, j] <- x[, j] - mean(X[, j])
}
y <- X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5] + rnorm(n)
y <- y - mean(y)
print(pca_regression(X, y, 3))
print(pca_regression(X, y, 5)$beta)
print(solve(t(X) %*% X) %*% t(X) %*% y)