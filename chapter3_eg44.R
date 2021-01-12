# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.44
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

# 犯罪データの読み込み
df <- read.table("./crime.txt")
bt_est <- as.list(NULL) # ブートストラップ推定値保存用リスト

# 統計量計算のモデル関数func_2()
for (j in 1:3) {
  func_2 <- function(data, index) {
    # j = 1, 2, 3でそれぞれ、線形モデルの切片、説明変数V3に対する傾き、説明変数V4に対する傾きを返す。
    return(coef(lm(V1 ~ V3 + V4, data = data, subset = index))[j])
  }
  bt_est[[j]] <- bt(df, func_2, 1000) # ブートストラップにより上記推定値の標準誤差を計算。
}
print(bt_est)

# オリジナルデータの表示
# 線形モデルの理論値で算出された標準誤差を確認する。
summary(lm(V1 ~ V3 + V4, data = df))