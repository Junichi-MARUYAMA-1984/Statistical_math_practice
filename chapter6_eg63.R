# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.63
# 一般化加法モデル（バックフィッティング）の実装。

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

# 多項式回帰を行う関数poly(x, y, z)
# x: 回帰モデル作成に用いるデータの説明変数値
# y: 回帰モデル作成に用いるデータの応答変数値
# z: 回帰を行う説明変数値。デフォルト引数としてxを設定する。
poly <- function(x, y, z = x) {
  n <- length(x) # 回帰モデル作成に用いるデータの数
  m <- length(z) # 回帰を行う説明変数データの数
  X <- cbind(rep(1, n), x, x ^ 2, x ^ 3) # 説明変数行列X（多項式回帰版）
  yy <- array(dim = m) # 多項式回帰推定値を記録する配列。
  beta_hat <- array(dim = 4) # 回帰係数推定値
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y # 回帰係数の最小二乗推定値の計算。
  X <- cbind(rep(1, m), z, z ^ 2, z ^ 3) # 多項式回帰推定値を計算するための説明変数行列を新たに定義。
  yy <- X %*% beta_hat # 多項式回帰推定値を計算。
  return(yy)
}

# 一般化加法モデルを用いた回帰の実際。
# バックフィッティングにより回帰を行う。

# データ生成
n <- 30
x <- runif(n) * 2 * pi - pi
y <- sin(x) + rnorm(n) * 0.5 # オリジナルよりノイズを減らした。
par(mfrow = c(1, 1)) # プロット面は通常通り1面で用いる。
plot(x, y, 
     ylim = c(-2, 2.5))
par(new = FALSE) # データ点プロットが回帰曲線により上書きされるのを防ぐ。

# 回帰処理
y_1 <- 0 # 多項式回帰推定値を記録する配列。
y_2 <- 0 # 局所線形回帰推定値を記録する配列。
for (k in 1:10) { # バックフィッティングの繰り返し回数
  y_1 <- poly(x, y - y_2) # 局所線形回帰推定値の残差に対して多項式回帰を実行。
  y_2 <- local(x, y - y_1) # 多項式回帰推定値の残差に対して局所線形回帰を実行。
}

# 回帰曲線のプロット
z <- seq(-3, 3, 0.1) # プロットを行うためのx値。
par(mfrow = c(1, 2)) # プロット面は2面で用いる。
plot(z, poly(x, y_1, z), type = "l", # 配列zの値に対する多項式回帰推定値をプロット。
     xlab = "x", ylab = "f(x)",
     ylim = c(-2, 2.5),
     main = "多項式回帰（3次）", col = "blue")
plot(z, local(x, y_2, z), type = "l", # 配列zの値に対する局所線形回帰推定値をプロット。
     xlab = "x", ylab = "f(x)",
     ylim = c(-2, 2.5),
     main = "局所線形回帰", col = "blue")
