# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.42
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

# J. Shao et alによる高速k-fold CV法の実装
cv_fast <- function(X, y, k) {
  n <- length(y)
  m <- n / k
  H <- X %*% solve(t(X) %*% X) %*% t(X) # ハット行列
  I_n <- diag(rep(1, n))
  e <- (I_n - H) %*% y # 残差を計算
  I_m <- diag(rep(1, m))
  S <- 0 # CVの各回における残差平方和の総和
  for (j in 1:k) {
    test <- ((j - 1) * m + 1):(j * m) # CVにおけるテストデータを選出
    S <- S + norm(solve(I_m - H[test, test]) %*% e[test], "2") ^ 2 # J. Shaoの式により残差平方和を計算してSに加える。
  }
  return(S / n) # サンプル数で残債平方和総和を標準化してCV結果として返す。
}

# cv_linear()とcv_fast()の処理速度を比較する。
# データ生成
n <- 3000 # データ数
p <- 5 # 説明変数の要素数
beta <- rnorm(p + 1)
x <- matrix(rnorm(n * p), ncol = p)
X <- cbind(1, x)
y <- X %*% beta + rnorm(n)
plot(0, 0, 
     xlab = "k",
     ylab = "実行時間",
     xlim = c(2, n),
     ylim = c(0, 1.0),
     type = "n")

# cv_fast()の処理
U <- NULL # k-fold CVのk値
V <- NULL # 各k値における処理時間
for (k in 10:n) {
  if (n %% k == 0) {
    t <- proc.time()[3]
    cv_fast(x, y, k)
    U <- c(U, k)
    V <- c(V, (proc.time()[3] - t))
  }
}
lines(U, V, col = "blue")

# cv_linear()の処理
U <- NULL
V <- NULL
for (k in 10:n) {
  if (n %% k == 0) {
    t <- proc.time()[3]
    cv_linear(x, y, k)
    U <- c(U, k)
    V <- c(V, (proc.time()[3] - t))
  }
}
lines(U, V, col = "red")

# グラフにレジェンドを追加する
legend("topleft", 
       legend = c("cv_linear", "cv_fast"),
       col = c("red", "blue"),
       lty = 1)