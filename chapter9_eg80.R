# Load library
library(e1071)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.80
# K-meansクラスタリング
k_means <- function(X, K, iteration = 20) {
  n <- nrow(X)
  p <- ncol(X)
  center <- array(dim = c(K, p))
  y <- sample(1:K, n, replace = TRUE)
  scores <- NULL
  for (h in 1:iteration) {
    for (k in 1:K) {
      if (sum(y[] == k) == 0) { # y[i] = kなるiの数を評価している
        center[k, ] <- Inf
      } else {
        for (j in 1:p) {
          center[k, j] <- mean(X[y[] == k, j])
        }
      }
    }
    S_total <- 0
    for (i in 1:n) {
      S_min <- Inf
      for (k in 1:K) {
        S <- sum((X[i, ] - center[k, ]) ^ 2)
        if (S < S_min) {
          S_min <- S
          y[i] <- k
        }
      }
      S_total <- S_total + S_min
    }
    scores <- c(scores, S_total)
  }
  return(list(clusters = y, scores = scores))
}

p <- 2
n <- 1000
X <- matrix(rnorm(p * n), nrow = n, ncol = p)
y <- k_means(X = X, K = 5)$clusters
plot(-3:3, -3:3, 
     xlab = "第1成分", ylab = "第2成分", type = "n")
points(X[, 1], X[, 2], col = y + 1)