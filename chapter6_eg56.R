# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.56
# 自然なスプラインによる回帰の実装

# 命題20で定義される関数d_j(x)
d <- function(j, x, knots) {
  K <- length(knots)
  return((max((x - knots[j]) ^ 3, 0) - max((x - knots[K]) ^ 3, 0)) / (knots[K] - knots[j]))
}
# 命題20で定義される関数h_j(x)
h <- function(j, x, knots) {
  K <- length(knots)
  if (j == 1) {
    return(1)
  } else if (j == 2) {
    return(x)
  } else {
    return(d(j - 2, x, knots) - d(K - 1, x, knots))
  }
}

# 以下では、
# 通常のスプライン回帰と、自然なスプラインによる回帰の曲線を比較する。
n <- 100
x <- rnorm(n) * 2 * pi
y <- sin(x) + 0.2 * rnorm(n)
K <- 11 # 数直線空間の区切り数
knots <- seq(-5, 5, length = K)

# 通常のスプライン回帰処理
X <- matrix(nrow = n, ncol = K + 4)
for (i in 1:n) {
  X[i, 1] <- 1
  X[i, 2] <- x[i]
  X[i, 3] <- x[i] ^ 2
  X[i, 4] <- x[i] ^ 3
  for (j in 1:K) {
    X[i, j + 4] <- max((x[i] - knots[j]) ^ 3, 0)
  }
}
beta <- solve(t(X) %*% X) %*% t(X) %*% y
f <- function(x) {
  S <- beta[1] + beta[2] * x + beta[3] * x ^ 2 + beta[4] * x ^ 3
  for (j in 1:K) {
    S <- S + beta[j + 4] * max((x - knots[j]) ^ 3, 0)
  }
  return(S)
}

# 自然なスプラインによる回帰処理
X <- matrix(nrow = n, ncol = K)
X[, 1] = 1
for (j in 2:K) {
  for (i in 1:n) {
    X[i, j] <- h(j, x[i], knots)
  }
}
gamma <- solve(t(X) %*% X) %*% t(X) %*% y
g <- function(x) {
  S <- gamma[1]
  for (j in 2:K) {
    S <- S + gamma[j] * h(j, x, knots)
  }
  return(S)
}

# グラフ描画
u_seq <- seq(-6, 6, 0.02)
v_seq <- NULL
for (u in u_seq) {
  v_seq <- c(v_seq, f(u))
}
plot(u_seq, v_seq,
     type = "l", col = "blue",
     yaxt = "n",
     xlab = "x", ylab = "f(x), g(x)")
par(new = TRUE)
w_seq <- NULL
for (u in u_seq) {
  w_seq <- c(w_seq, g(u))
}
plot(u_seq, w_seq,
     type = "l", col = "red",
     yaxt = "n",
     xlab = "", ylab = "")
par(new = TRUE)
points(x, y)
abline(v = knots, lty = 3)
abline(v = c(-5, 5), lwd = 2)
title("K = 11")
legend(-3.7, 1.1, c("スプライン", "自然なスプライン"),
       lty = 1,
       col = c("blue", "red"))