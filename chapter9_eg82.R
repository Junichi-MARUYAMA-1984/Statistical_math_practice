# Load library
library(e1071)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.82
# K-meansクラスタリングの初期値依存性を検討
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
input <- 1:20
output <- k_means(X, 5)$scores
plot(input, log(output),
     ylim = c(6.2, 7.5),
     xlab = "繰り返し回数", ylab = "log(スコア)",
     type = "l", col = 1,
     main = "初期値ごとにスコアの変化を見る")
for (r in 2:10) {
  output <- k_means(X, 5)$scores
  lines(input, log(output),
        col = r)
}