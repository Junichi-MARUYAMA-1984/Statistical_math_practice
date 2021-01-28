# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
par(family= "HiraKakuProN-W3")

# Chapter6 e.g.60
# Nadaraya-Watson推定量はカーネル密度推定の回帰版であり、
# ノンパラメトリックな回帰手法の一つである。
# 本コードではNadaraya-Watson推定量にEpanechnikovカーネルを適用する。
# コードの処理内容を追うと分かるが、Nadaraya-Watson推定量を用いた回帰は、
# 応答変数yの値が説明変数xに対して一価関数的な場合にうまく機能する。

# データ生成
n <- 250
x <- 2 * rnorm(n)
y <- sin(2 * pi * x) + rnorm(n) / 4 # y = sin(2pi * x)という一価関数でデータを生成する。

# Epanechnikovカーネルの定義式中における関数D(t)
D <- function(t) {
  return(max(0.75 * (1 - t ^ 2), 0))
}

# Epanechnikovカーネルの定義式K_{lambda}(x, y)
K <- function(x, y, lambda) {
  return(D(abs(x - y) / lambda))
}

# Nadaraya-Watson推定量の定義式f(x)
# 今回はEpanechnikovカーネルをKとして考えるので、引数にlambdaが入る。
f <- function(z, lambda) { 
  numer <- 0 # fの分子
  denom <- 0 # fの分母
  for (i in 1:n) {
    numer <- numer + K(x[i], z, lambda) * y[i] # 定義式中の分子を計算。
    denom <- denom + K(x[i], z, lambda) # 定義式中の分母を計算。
  }
  return(numer / denom)
}

# lambda = 0.05, 0.25としてNadaraya-Watson推定量を計算し、プロットする。
plot(seq(-3, 3, length = 10), # x軸のticks
     seq(-2, 3, length = 10), # y軸のticks
     type = "n", # このplot()では軸だけ描く。データはプロットしない。
     xlab = "x", ylab = "y")
points(x, y) # データのプロット
xx <- seq(-3, 3, 0.1) # Nadaraya-Watson推定量で回帰を行うためのx値。
yy <- NULL # 各x値に対するNadaraya-Watson推定量を記録する配列。
for (zz in xx) {
  yy <- c(yy, f(zz, 0.05)) # lambda = 0.05としてNadaraya-Watson推定量を計算。
}
lines(xx, yy, col = "green") # 緑色の線でlambda = 0.05の結果をプロット。
yy <- NULL
for (zz in xx) {
  yy <- c(yy, f(zz, 0.25)) # lambda = 0.25としてNadaraya-Watson推定量を計算。
}
lines(xx, yy, col = "blue") # 青色の線でlambda = 0.25の結果をプロット。

# Nadaraya-Watson推定量を計算する際の最適なlambdaをCVにより求める。
m <- n / 10 # 10-fold CVとする。
lambda_seq <- seq(0.05, 1, 0.01) # 評価するlambda値の範囲。
SS_min <- Inf # 最小CV二乗誤差
for (lambda in lambda_seq) {
  SS <- 0 # 評価しようとするlambda値におけるCV二乗誤差
  for (k in 1:10) { # 10-fold CV
    test <- ((k - 1) * m + 1):(k * m) # テスト用データの添字
    train <- setdiff(1:n, test) # 学習用データの添字
    for (j in test) {
      numer_j <- 0 # Nadaraya-Watson推定量定義式の分子
      denom_j <- 0 # Nadaraya-Watson推定量定義式の分母
      for (i in train) {
        K_ij <- K(x[i], x[j], lambda) # テストデータjと学習データiの間のEpanechnikovカーネル値を計算。
        numer_j <- numer_j + K_ij * y[i] # テストデータjにおけるNadaraya-Watson推定量の分子を計算。
        denom_j <- denom_j + K_ij # テストデータjにおけるNadaraya-Watson推定量の分母を計算。
      }
      if (denom_j == 0) { # テストデータjと学習データ達がlambda値に比して極端に離れていた場合、こうなる可能性がある。
        d_min <- Inf # テストデータjと各学習データとのEuclid距離の最小値
        for (i in train) {
          d <- abs(x[j] - x[i]) # テストデータjと各学習データとのEuclid距離を計算。
          if (d < d_min) {
            d_min <- d
            index <- i # テストデータと最もEuclid距離の近い学習データの添字を記録。
          }
        }
        z <- y[index] # Nadaraya-Watson推定量が計算できない場合は、テストデータと最もEuclid距離の近い学習データxにおけるy値を推定値とする。
      } else {
        z <- numer_j / denom_j # Nadaraya-Watson推定量が計算できる場合は、それを推定値とする。
      }
      SS <- SS + (y[j] - z) ^ 2 # 各テストデータy値とその推定値間の二乗誤差を計算し、総和を記録する。これが当該lambda値におけるCV評価値となる。
    }
  }
  print(SS) # CV評価値を標準出力へ。
  if (SS < SS_min) {
    SS_min <- SS
    lambda_best <- lambda # 最小CV評価値を叩き出したlambda値（=最適lambda値）を記録。
  }
}

# 最適lambda値を用いてNadaraya-Watson推定量を計算し、プロットする。
yy <- NULL # 各x値に対するNadaraya-Watson推定量を記録する配列。
for (zz in xx) {
  yy <- c(yy, f(zz, lambda_best)) # 最適lambdaでのNadaraya-Watson推定量を計算。
}
lines(xx, yy, col = "red") # 赤い直線でプロットする。
title("Nadaraya-Watson推定量")
legend("topleft", 
       legend = paste0("lambda = ", c(0.05, 0.25, "lambda_best")),
       lwd = 1,
       col = c("green", "blue", "red"))