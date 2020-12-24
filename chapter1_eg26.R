# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.26
r2 <- function(x, y) {
  y_hat <- lm(y ~ x)$fitted.values
  print(lm(y ~ x))
  y_bar <- mean(y)
  RSS <- sum((y - y_hat)^2)
  TSS <- sum((y - y_bar)^2)
  return(1 - RSS / TSS)
}

# ケース1
N <- 100
m <- 2
x <- matrix(rnorm(m * N), ncol = m) # xはN(0, 1)に従う2個の説明変数からなるデータ100組の行列。
y <- rnorm(N) # yはN(0, 1)に従う100個の乱数からなるベクトル。
r2(x, y) # 決定係数。xとyの間に線形従属関係はないので、決定係数は小さくなる。

# ケース2
N <- 100
m <- 1
x <- matrix(rnorm(m * N), ncol = m) # xはN(0, 1)に従う100個の乱数からなるベクトル。
y <- rnorm(N) # yもN(0, 1)に従う100個の乱数からなるベクトル。
r2(x, y) # 決定係数。xとyの間に線形従属関係はないので、決定係数は小さくなる。
cor(x, y)^2 # 単回帰の場合、決定係数はPearson積率相関係数の二乗と等しくなる。

# ケース3（独自）
N <- 100
m <- 2
x <- matrix(rnorm(m * N), ncol = m) # xはN(0, 1)に従う2個の説明変数からなるデータ100組の行列。
x_mat <- cbind(1, x) # 説明変数行列を完成させる。
beta <- c(1, 1, 2) # xとyを関係付けるパラメタ。
epsilon <- rnorm(N) # 測定誤差値
y <- x_mat %*% beta + epsilon # パラメタbetaによりxからyを生成。
r2(x, y) # 決定係数。xとyの間には線形従属関係があるので、決定係数は大きくなる。