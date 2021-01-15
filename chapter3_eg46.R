# Load library
library(MASS)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.46

# RSS_k = min_{k(S) = k} RSS(S) を算出する関数。
# k_matrixは、集合{1, ..., p}における大きさkの部分集合を列に持つ、
# サイズk x pCk（pCkは二項係数(p k)^Tのこと）の行列。
RSS_min <- function(X, y, k_matrix) {
  m <- ncol(k_matrix) # RSSを評価する説明変数集合Sの数
  n_ <- length(y) # サンプル数
  S_min <- Inf # RSS(S)の最小値
  for (j in 1:m) {
    q <- k_matrix[, j] # 説明変数集合Sを取得。
    S <- sum((lm(y ~ X[, q])$fitted.values - y) ^ 2) / n_ # RSS(S)を計算。
    if (S < S_min) { # より小さいRSS(S)が出てきたら、S_minとその時の説明変数集合set_qを更新。
      S_min <- S
      set_q <- q
    }
  }
  return(list(value = S_min, set = set_q))
}

# Bostonデータを用いて、AICとBICの値の挙動をプロットしてみる。
df <- Boston
X <- as.matrix(df[, c(1, 3, 5, 6, 7, 8, 10, 11, 12, 13)])
y <- df[[14]]
n <- nrow(X)
p <- ncol(X)
AIC_seq <- NULL; BIC_seq <- NULL
for (k in 1:p) {
  k_matrix <- combn(1:p, k) # 集合{1, ..., p}における大きさkの部分集合を列に持つ行列k_matrixを生成。
  res <- RSS_min(X, y, k_matrix) # RSS_kを計算。
  AIC <- n * log(res$value / n) + 2 * k # k(S) = kの際のAICを計算。
  BIC <- n * log(res$value / n) + k * log(n) # k(S) = kの際のBICを計算。
  AIC_seq <- c(AIC_seq, AIC)
  BIC_seq <- c(BIC_seq, BIC)
}
plot(1:p, ylim = c(min(AIC_seq), max(BIC_seq)), 
     type = "n", 
     xlab = "# of variables",
     ylab = "IC values")
lines(AIC_seq, col = "red")
lines(BIC_seq, col = "blue")
legend("topright", legend = c("AIC", "BIC"),
       col = c("red", "blue"),
       lwd = 1,
       cex = 0.8)