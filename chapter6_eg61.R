# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.61
# 局所線形回帰の実装。
# 本コードではEpanechnikovカーネルを用いて回帰を行う。

# Epanechnikovカーネルの定義式における関数D(t)
D <- function(t) {
  return(max(0.75 * (1 - t ^ 2), 0))
}

# Epanechnikovカーネルの定義式K_{lambda}(x, y)
K <- function(x, y, lambda) {
  return(D(abs(x - y) / lambda))
}

# 局所線形回帰を実行する関数local(x, y, z)
# x: 回帰モデル作成に用いるデータの説明変数値
# y: 回帰モデル作成に用いるデータの応答変数値
# z: 回帰を行う説明変数値。デフォルト引数としてxを設定する。
local <- function(x, y, z = x) {
  X <- cbind(rep(1, n), x) # 説明変数行列
  yy <- NULL # 局所線形回帰による推定値
  beta_hat <- array(dim = 2) # 回帰係数推定値。局所線形回帰においてはxの関数となる。
  for (u in z) { # 各テストデータごとにbeta_hatを計算する。
    w <- array(dim = n) # カーネルk(u, x[i])の値を記録する配列。
    for (i in 1:n) {
      w[i] <- K(u, x[i], lambda = 1) # lambda = 1としてEpanechnikovカーネル値を計算。
    }
    W <- diag(w) # 式(6.8)の行列（カーネル値が対角に並ぶ行列）。
    beta_hat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y # 回帰係数の最小二乗推定値を計算。
    yy <- c(yy, beta_hat[1] + beta_hat[2] * u) # 局所線形回帰推定値を計算。
  }
  return(yy)
}

# 実際に局所線形回帰を実行する。

# データの生成
n <- 30
x <- runif(n) * 2 * pi - pi
y <- sin(x) + rnorm(n) * 0.5 # オリジナルのコードよりもノイズを減らした。
plot(x, y)

# 回帰の実行
m <- 200 # 局所線形回帰推定値を計算するx値を設定するためのパラメタ。
U <- seq(-pi, pi, pi / m) # 局所線形回帰推定値を計算するx値の設定。
V <- local(x, y, U) # 局所線形回帰推定値を計算。
lines(U, V, col = "red", type = "l")
title("局所線形回帰 (p = 1, N = 30)")