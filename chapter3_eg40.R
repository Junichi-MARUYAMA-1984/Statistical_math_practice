# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.40
# k-fold クロスバリデーション（CV）の実装。
# サンプル全体をk個のグループに分ける。
# うち1グループをテストデータとして確保し、残りを訓練データとして用いて、
# 線形回帰の回帰係数推定値を計算する。
# ここで計算された回帰係数推定値を用いて、
# テストデータにおける応答変数推定値y_hatと実際の応答変数値yのRSSを計算する。
# その処理をテストデータとして用いるグループを変えることでk回行い、
# k回分のRSS値の和を総サンプル数nで標準化した値を最終結果として返す。
cv_linear <- function(X, y, k) {
  n <- length(y)
  m <- n / k # 各グループのサンプル数
  S <- 0 # RSS和
  for (j in 1:k) {
    test <- ((j - 1) * m + 1):(j * m) # テストデータグループの選択
    beta <- solve(t(X[-test, ]) %*% X[-test, ]) %*% t(X[-test, ]) %*% y[-test] # 回帰係数推定値の計算
    e <- y[test] - X[test, ] %*% beta # 残差y - y_hatを計算
    S <- S + drop(t(e) %*% e) # eの各要素の二乗和を計算し、スカラー値に変換してからSに加える。
  }
  return(S/n)
}

# k-fold CVのk値を変えての比較。
# サンプル数100。
# 説明変数X[, c(2:6)]は5個の要素からなる。
# （X[, 1]には、線形回帰式の切片項を計算するための値である1が入る）
# 応答変数yの正解値は、X %*% beta + epsで生成させる。
# 誤差epsの各要素はN(0, 1)でランダムに与える。
# 係数betaの各要素はN(0, 1)でランダムに与える。
# この状況下でkの値を2:100の間で動かし（kは100の約数のみ有効とする）、
# k-fold CVを行って、その結果をプロットし線で結ぶ。
# この試行は10回行い、色を変えて10回分の結果をプロットさせることとする。
n <- 100
p <- 5
plot(0, 0,
     xlab = "k",
     ylab = "CVの値",
     xlim = c(2, n),
     ylim = c(0.3, 1.5),
     type = "n")
for (j in 2:11) {
  X <- matrix(rnorm(n * p), ncol = p); X <- cbind(1, X)
  beta <- rnorm(p + 1)
  eps <- rnorm(n)
  y <- X %*% beta + eps
  U <- NULL
  V <- NULL
  for (k in 2:n) {
    if (n %% k == 0) {
      U <- c(U, k)
      V <- c(V, cv_linear(X, y, k))
    }
  }
  points(U, V, col = j)
  lines(U, V, col = j)
}