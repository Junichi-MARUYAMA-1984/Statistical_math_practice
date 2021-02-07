# Load library
library(quadprog)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter8 e.g.73
# サポートベクトルマシン
# X: 説明変数行列
# y: 応答変数ベクトル
# C: サポートベクトルマシンの主問題で定義されるコストC(>0)
svm_1 <- function(X, y, C) {
  eps <- 0.0001 # 「十分小さな値」として用いる定数
  n <- nrow(X)
  meq <- 1 # 制約式Amat %*% alpha >= bvec のうち、最初のmeq個の式は等式とする。
  Dmat <- matrix(nrow = n, ncol = n) # quadprogで計算する際に定義されるDmat
  for (i in 1:n) {
    for (j in 1:n) {
      Dmat[i, j] <- sum(X[i, ] * X[j, ]) * y[i] * y[j] # Dmatの定義式に則って計算
    }
  }
  Dmat <- Dmat + eps * diag(n) # 対角成分に小さな値を加えて、正則にする。
  dvec <- rep(1, n) # quadprogで計算する際に定義されるdvec
  Amat <- matrix(nrow = 2 * n + 1, ncol = n) # quadprogで計算する際に定義されるAmat
  Amat[1, ] <- y # Amatの定義式に則って代入。
  Amat[2:(n + 1), 1:n] <- -diag(n) # Amatの定義式にそって値を代入。
  Amat[(n + 2):(2 * n + 1), 1:n] <- diag(n) # Amatの定義式にそって値を代入。
  Amat <- t(Amat) # このパッケージでは、Amatは転置にしたものを指定する。
  bvec <- c(0, rep(-C, n), rep(0, n)) # quadprogで計算する際に定義されるbvec
  alpha <- solve.QP(Dmat = Dmat, dvec = dvec, 
                    Amat = Amat, bvec = bvec, meq = 1)$solution # quadprogの関数solve.QP()でalphaを計算。
  beta <- drop((alpha * y) %*% X) # alphaからbetaを計算。
  index <- (1:n)[eps < alpha & alpha < C - eps] # 0 < alpha_{i} < Cなるalpha_{i}の添字集合を求める。数値計算誤差を考慮してepsによる閾値を設定している。
  beta_0 <- mean(y[index] - X[index, ] %*% beta) # beta_0を計算。
  return(list(beta = beta, beta_0 = beta_0))
}

# 実際にサポートベクトルマシン処理を実行する。
a <- rnorm(1)
b <- rnorm(1)
n <- 100
X <- matrix(rnorm(n * 2), ncol = 2, nrow = n) # 説明変数行列の作成
y <- sign(a * X[, 1] + b * X[, 2] + 0.1 * rnorm(n)) # 応答変数ベクトルの作成。値は-1か1を取る。
plot(-3:3, -3:3,
     xlab = "第1成分", ylab = "第2成分",
     type = "n")
for (i in 1:n) {
  if (y[i] == -1) {
    points(X[i, 1], X[i, 2], col = "red") # 応答変数値が-1なら点の色を赤とする。
  } else {
    points(X[i, 1], X[i, 2], col = "blue") # 応答変数値が1なら点の色を青とする。
  }
}
qq <- svm_1(X, y, 10) # サポートベクトルマシン処理を実行。
abline(-qq$beta_0 / qq$beta[2], -qq$beta[1] / qq$beta[2]) # サポートベクトルマシンで得られた境界線を描画。