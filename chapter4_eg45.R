# Load library
library(MASS)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter4 e.g.45
# 赤池情報量基準(AIC)の実装。

# RSS_k = min_{k(S) = k} RSS(S) を算出する関数。
# k_matrixは、集合{1, ..., p}における大きさkの部分集合を列に持つ、
# サイズk x pCk（pCkは二項係数(p k)^Tのこと）の行列。
RSS_min <- function(X, y, k_matrix) {
  m <- ncol(k_matrix) # RSSを評価する説明変数集合Sの数
  S_min <- Inf # RSS(S)の最小値
  for (j in 1:m) {
    q <- k_matrix[, j] # 説明変数集合Sを取得。
    S <- sum((lm(y ~ X[, q])$fitted.values - y) ^ 2) # RSS(S)を計算。
    if (S < S_min) { # より小さいRSS(S)が出てきたら、S_minとその時の説明変数集合set_qを更新。
      S_min <- S
      set_q <- q
    }
  }
  return(list(value = S_min, set = set_q))
}

# Bostonデータをもとに、AIC算出処理を実装する。
df <- Boston
X <- as.matrix(df[, c(1, 3, 5, 6, 7, 8, 10, 11, 12, 13)]) # 説明変数行列
y <- df[[14]] # 応答変数の正解
p <- ncol(X) # 説明変数行列に含まれる説明変数の数
n <- length(y) # データ数
AIC_min <- Inf # AICの最小値
for (k in 1:p) {
  k_matrix <- combn(1:p, k) # 集合{1, ..., p}における大きさkの部分集合を列に持つ行列k_matrixを生成。
  res <- RSS_min(X, y, k_matrix) # RSS_kを計算。
  AIC <- n * log(res$value / n) + 2 * k # k(S) = kの時のAICを計算。
  if (AIC < AIC_min) { # より小さいAICが出てきたら、AIC_minとその時の説明変数集合set_minを更新。
    AIC_min <- AIC
    set_min <- res$set
  }
}
print(AIC_min)
print(set_min)

# Bostonデータをもとに、自由度調整済み決定係数算出処理を実装する。
y_bar <- mean(y)
TSS <- sum((y - y_bar) ^ 2) # 全変動total sumを計算。
D_max <- -Inf # 自由度調整済み決定係数の最大値
for (k in 1:p) {
  k_matrix <- combn(1:p, k) # 集合{1, ..., p}における大きさkの部分集合を列に持つ行列k_matrixを生成。
  res <- RSS_min(X, y, k_matrix) # RSS_kを計算。
  if ((n - k - 1) != 0) {
    frac_u <- res$value / (n - k - 1) # RSS_k / (N - k - 1)を計算。
  }
  if ((n - 1) != 0) {
    frac_l <- TSS / (n - 1) # TSS / (N - 1)を計算。
  }
  if (frac_l != 0) {
    D <- 1 - frac_u / frac_l # 調整済み決定係数を計算。
    # D <- 1 - res$value / (n - k - 1) / TSS * (n - 1) # 鈴木テキスト的な調整済み決定係数の立式。
  }
  if (D > D_max) { # より大きなDが出てきたら、D_maxとその時の説明変数集合set_maxを更新。
    D_max <- D
    set_max <- res$set
  }
}
print(D_max)
print(set_max)
