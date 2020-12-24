# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.34
# 説明変数xの値として、
# N(1, 1)に従った乱数を100個、N(-1, 1)に従った乱数を100個用意する。
# 応答変数yは{-1, 1}の二値のみを取るとして、
# xがN(1, 1)に従う乱数の場合は1, N(-1, 1)に従う乱数の場合は-1となるように
# データ組(x, y)を作成する。組数は200となる。
# 直感的には、xが正値の時はyは1、xが負値の時はyは-1を取るようなデータである。
# この200組のデータのうちランダムに100組を取り出して、
# ロジスティックモデルによる回帰を行う。
n <- 100 
x <- c(rnorm(n) + 1, rnorm(n) - 1)
y <- c(rep(1, n), rep(-1, n))
train <- sample(1:(2 * n), n, replace = FALSE) # 1:200の中からランダムに100個の整数を取り出す。
df <- data.frame(x, y)
x <- as.matrix(df[train, 1]); y <- as.vector(df[train, 2]) # 200組の(x, y)データからランダムに100組を取り出す。
p <- 1
X <- cbind(1, x)

# Newton-Raphson法による回帰パラメタの最尤推定
beta_hat <- Inf
gamma_hat <- rnorm(p + 1)
while (sum((beta_hat - gamma_hat)^2) > 0.001) {
  beta_hat <- gamma_hat
  s <- as.vector(X %*% beta_hat)
  v <- exp(-s * y)
  u <- y * v / (1 + v)
  w <- v / (1 + v)^2
  W <- diag(w)
  z <- s + u / w
  gamma_hat <- as.vector(solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z)
  print(gamma_hat)
}

# 作成したロジスティック回帰モデルの性能を確認。
# 
# モデルにより定義される「y = -1となる確率1 / (1 + exp(s))」を考えたとき、
# 式中の指数部sの符号が負の時は確率が大きくなり、
# 正の時は確率が小さくなる傾向があることが推測される。
# よって、sの符号を見ることで簡易的に「モデルによるyの推定値」を求めることができる。
# すなわち、sが正値ならばモデルによるyの推定値は1であり、
# sが負値ならばモデルによるyの推定値は-1であると判定する。
# この「モデルによるyの推定値」と実際のyの値の一致率を解析することで、
# 回帰モデルの性能を検討する。
x <- as.matrix(df[-train, 1]); y <- as.vector(df[-train, 2])
z <- 2 * as.integer(gamma_hat[1] + x * gamma_hat[2] > 0) - 1
print(table(y, z))
