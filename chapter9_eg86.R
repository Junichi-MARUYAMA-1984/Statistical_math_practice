# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.86
# 主成分分析の実装
pca <- function(x) {
  n <- nrow(x)
  p <- ncol(x)
  center <- array(dim = p)
  for (j in 1:p) {
    center[j] <- mean(x[, j])
    x[, j] <- x[, j] - center[j]
  }
  sigma <- t(x) %*% x / n
  lambda <- eigen(sigma)$values
  phi <- eigen(sigma)$vectors
  return(list(lambdas = lambda, vectors = phi, centers = center))
}

n <- 100
p <- 5
x <- matrix(rnorm(n * p), ncol = p, nrow = n)
print(pca(x)$lambdas)
print(pca(x)$vectors)
print(pca(x)$centers)
print(prcomp(x)$rotation)
print((prcomp(x)$sdev) ^ 2)
print(prcomp(x)$center)
print(names(prcomp(x)))

pr_var <- (prcomp(x)$sdev) ^ 2
pve <- pr_var / sum(pr_var)
par(mfrow = c(1, 2))
plot(pve, 
     xlab = "主成分", ylab = "寄与率",
     ylim = c(0, 1), type = "b")
plot(cumsum(pve), 
     xlab = "主成分", ylab = "累積寄与率",
     ylim = c(0, 1), type = "b")
par(mfrow = c(1, 1))