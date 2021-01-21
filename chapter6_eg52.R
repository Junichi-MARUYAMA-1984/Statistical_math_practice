# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.52
# 多項式回帰の実装。
n <- 100
x <- rnorm(n) # 標準正規分布に従う乱数を100個生成
y <- sin(x) + rnorm(n) # sin(x)の値にさらに標準正規分布に従うノイズを加える。
m <- 3 # 多項式回帰の種類数。今回は3次、5次、7次の三種類の多項式回帰を行う。
p_set <- c(3, 5, 7)
col_set <- c("red", "blue", "green")

# 係数ベクトルbetaの時の多項式回帰式におけるx = uの式の値。
g <- function(beta, u) {
  S <- beta[1]
  for (j in 1:p) {
    S <- S + beta[j + 1] * u ^ j
  }
  return(S)
}

# 多項式回帰
for (i in 1:m) {
  p <- p_set[i]
  X <- rep(1, n)
  for (j in 1:p) {
    X <- cbind(X, x ^ j) # 多項式回帰式の各項におけるx^pの値を計算
  }
  beta <- drop(solve(t(X) %*% X) %*% t(X) %*% y) # betaの最小二乗推定値を計算
  f <- function(u) { # x = uの時の回帰式の値を返す関数を定義
    g(beta, u)
  }
  curve(f(x), -3, 3, col = col_set[i], yaxt = "n") # yaxt = "n"により、y軸を描画しない。
  par(new = TRUE)
}
points(x, y)
legend("topleft", lty = 1, paste0("p = ", p_set), col = col_set)
