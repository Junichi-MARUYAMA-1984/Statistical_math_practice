# Load library
library(ISLR)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.43
# ブートストラップ法の実装。
bt <- function(df, f, r) {
  m <- nrow(df) # データフレーム中のデータ数
  org <- f(df, 1:m) # データフレーム中の全てのデータを用いて計算した統計量（オリジナル推定値）。
  u <- array(dim = r) # ブートストラップ標本から計算した統計量（ブートストラップ推定値）を保存する配列。
  for (j in 1:r) { # r回リサンプリングを繰り返す。
    index <- sample(1:m, m, replace = TRUE) # ブートストラップ標本を作成。
    u[j] <- f(df, index) # ブートストラップ標本を用いて統計量を計算。
  }
  return(list(original = org, # オリジナル推定値
              bias = mean(u) - org, # ブートストラップ推定値の平均とオリジナル推定値の間の差。元データの偏りを表す。
              stderr = sd(u))) # ブートストラップ推定値の標準偏差。オリジナル推定値の標準誤差を表していると考えられる。
}

# 統計量計算のモデル関数func_1()
func_1 <- function(data, index) {
  X <- data$X[index] # df$Xのうち、配列indexで指定されたデータを計算に使用する。
  Y <- data$Y[index] # df$Yのうち、配列indexで指定されたデータを計算に使用する。
  return((var(Y) - var(X)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}

# ブートストラップ推定結果
bt(Portfolio, func_1, 1000)
