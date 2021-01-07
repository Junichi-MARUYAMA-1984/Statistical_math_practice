# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.39
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
    S <- S + drop(t(e) %*% e) # eのL2ノルム（RSS）を計算し、スカラー値に変換してからSに加える。
  }
  return(S/n)
}

# k-fold CVの実際。
# サンプル数100。
# 説明変数X[, c(2:6)]は5個の要素からなる。
# （X[, 1]には、線形回帰式の切片項を計算するための値である1が入る）
# 応答変数yの正解値は、X %*% beta + epsで生成させる。
# 係数betaの各要素はN(0, 1)でランダムに与えるが、
# beta[2]とbeta[3]は人為的に0とする。
# すなわち、応答変数yは説明変数中の1番目と2番目の要素には依存しないとする。
# この状況下で、
# i) 説明変数中の3～5番目の要素（すなわちX[, c(1, 4, 5, 6)]）を用いての線形回帰。
# と、
# ii) 説明変数中の全ての要素（すなわちX）を用いての線形回帰
# を行い、それぞれの10-fold CVの結果をプロットさせる。
# この試行は100回行い、100回分の比較結果をプロットさせることとする。
n <- 100
p <- 5
X <- matrix(rnorm(n * p), ncol = p); X <- cbind(1, X)
beta <- rnorm(p + 1)
beta[c(2, 3)] <- 0
U <- NULL
V <- NULL
for (j in 1:100) {
  eps <- rnorm(n)
  y <- X %*% beta + eps
  U <- c(U, cv_linear(X[, c(1, 4, 5, 6)], y, 10))
  V <- c(V, cv_linear(X, y, 10))
}
plot(U, V, 
     xlab = "cv_linear(X[, c(1, 4, 5, 6)], y, 10)",
     ylab = "cv_linear(X, y, 10)",
     main = "変数を多く選びすぎて過学習するシミュレーション")
abline(a = 0, b = 1, col = "red") # 切片0、傾き1の直線を赤色で引く。