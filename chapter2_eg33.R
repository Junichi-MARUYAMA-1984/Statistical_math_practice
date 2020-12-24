# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.33
# N(0, 1)に従う要素二個からなる説明変数xを用いて
# ロジスティックモデルにより以下のコードで確率probを構成し、
# {-1, 1}の二値のみを取る応答変数yが-1を取る確率を定義する。
# 実際に構成した確率probにより応答変数yの値を決める。
# このような(x, y)のデータを1000組用意する。
N <- 1000 # データ数
p <- 2 # 説明変数の要素は2個。
X <- matrix(rnorm(N * p), ncol = p); X <- cbind(rep(1, N), X) # 説明変数行列の作成。
beta <- rnorm(p + 1) # 回帰パラメタ（切片も含めて3個ある）。今回はN(0, 1)に従う乱数で決定。
y <- array(N) # 確率probに従って決定されるyの値を格納する配列。
s <- as.vector(X %*% beta) # ロジスティックモデルのexpの内部を計算。
prob <- 1 / (1 + exp(s)) # ロジスティックモデルにより定義される確率。
                         # この確率probに従ってyが-1を取るとする。
# 区間[0, 1]における一様乱数を一個runif(1)で作成し、
# その値とprob[i]を比較する。
# prob[i]も区間[0, 1]に値を取るので、
# 「runif(1)よりもprob[i]が大きな値を取る確率」がprob[i]の値そのものとなる。
# よってこの比較により、確率probに従って応答変数yの値を決定することができる。
for (i in 1:N) {
  if (runif(1) < prob[i]) {
    y[i] <- -1
  } else {
    y[i] <- 1
  }
}

# 最尤推定
beta_hat <- Inf
gamma_hat <- rnorm(p + 1)
# Newton-Raphson法によるbeta推定値の計算。
# ステップ間のbeta要素推定値の差の二乗和が0.001を下回ったら収束したと判断する。
while (sum((beta_hat - gamma_hat)^2) > 0.001) {
  beta_hat <- gamma_hat
  s <- as.vector(X %*% beta_hat)
  v <- exp(-s * y)
  u <- y * v / (1 + v)
  w <- v / (1 + v)^2
  W <- diag(w)
  z <- s + u / w # Wの逆行列は対角線上に1/wが並んだものになるので、
                 # zの計算はこの式の通りとなる。
  gamma_hat <- as.vector(solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z)
  print(gamma_hat)
}

# 結果出力
print(beta) # ロジスティックモデルにおけるパラメタの真値。
print(gamma_hat) # 最尤推定により計算されたパラメタ推定値。