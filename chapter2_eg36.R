# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.36
# Fisherのアヤメのデータ(iris)について、
# 4要素を持つ説明変数x（がく辺の長さ、がくの幅、花弁の長さ、花弁の幅）から、
# 3要素を持つ応答変数であるアヤメの種類（1: setosa, 2: versicolor, 3: virginica）を判別する。
# 簡単のため、今回は各アヤメ種の事前確率が等確率であると仮定する。

# 各アヤメ種iのもとでの説明変数xが、以下の確率密度関数（多次元正規分布）
# f_i(x) = {(2pi)^p * det(cov(x))}^(-1/2) * exp{(-1/2) * t(x - mu_i) %*% solve(cov(x)) %*% (x - mu_i)}
# に従うものと仮定する。
# このf_iにおいて、
# 係数{(2pi)^p}^(-1/2)を無視し、両辺のlogを取った式の右辺を、
# 以下のコードで関数fとして定義した。
# この先の処理で知りたいのは各アヤメ種の事後確率の境界線なので、
# このように変形した関数を用いて構わない。
f <- function(w, mu, inv, de) {
  -0.5 * (w - mu) %*% inv %*% t(w - mu) - 0.5 * log(de) # w - muは行ベクトル（正確にはmatrix[1:4]）であることに注意。
}

# 学習用データの前処理
df <- iris # irisデータの読み込み
df[[5]] <- c(rep(1, 50), rep(2, 50), rep(3, 50)) # アヤメ種の名前を数字(1, 2, 3)に変更
n <- nrow(df) # irisのデータ数（150個）
train <- sample(1:n, n / 2, replace = FALSE) # 1:150の中からランダムに75個の整数を取り出す。
test <- setdiff(1:n, train) # 1:150の中からtrainとして選択されていない整数75個を取得する。
mat <- as.matrix(df[train,]) # trainで選択された整数に対応するデータをirisから学習用データとして取り出す。

# 学習用データを用いて、各アヤメ種のもとでの説明変数の確率分布パラメタ推定値を計算。
mu <- list() # 各アヤメ種のもとでの説明変数の推定平均値
covv <- list() # 各アヤメ種のもとでの説明変数の推定分散共分散行列
for (j in 1:3) {
  x <- mat[mat[, 5] == j, 1:4] # アヤメ種の番号に対応する説明変数データを学習用データより取り出す。
  mu[[j]] <- c(mean(x[, 1]), mean(x[, 2]), mean(x[, 3]), mean(x[, 4])) # 説明変数の平均値を計算し、mu[[j]]に保存。
  covv[[j]] <- cov(x) # 説明変数の分散共分散行列を計算し、covv[[j]]に保存。
}

# 各アヤメ種のもとで説明変数値vを取る確率を計算するための汎関数gを定義
g <- function(v, j) {
  f(v, mu[[j]], solve(covv[[j]]), det(covv[[j]]))
}

# テストデータにおけるアヤメ種判別処理
z <- array(dim = n / 2) # 判別結果（アヤメ種の番号）保存用配列
k <- 1 # zのインデックス変数
prob <- array(1:3) # 各アヤメ種のもとでその説明変数値を取る確率
for (i in test) {
  u <- as.matrix(df[i, 1:4]) # 説明変数データの取得
  for (j in 1:3) {
    prob[j] <- g(u, j) # jがアヤメ種の種番号に対応する。
  }
  z[k] <- which.max(prob) # 最も確率の高いアヤメ種の種番号を保存
  k <- k + 1
}
table(z, df[test, 5]) # 判別結果と正解データの比較