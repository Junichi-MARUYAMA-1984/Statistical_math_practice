# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.53
# 一般の多項式回帰

# 偶関数に近いデータの作成。
# 鈴木先生の修正でceiling()を用いているが、これだと奇関数になってしまう…。
n <- 100
x <- rnorm(n) * pi
y <- ceiling(x) %% 2 * 2 - 1 + rnorm(n) * 0.2
plot(x, y, 
     xaxt = "n", yaxt = "n",
     xlim = c(-5, 5), ylim = c(-2, 2),
     # ann = FALSE, 
     main = "偶関数の乱数をsin, cosで追従")

# 基底としてcos(x)を使用した場合の多項式回帰。
X <- cbind(1, cos(x), cos(2 * x), cos(3 * x))
beta <- solve(t(X) %*% X) %*% t(X) %*% y
f <- function(x) {
  beta[1] + beta[2] * cos(x) + beta[3] * cos(2 * x) + beta[4] * cos(3 * x)
}
par(new = TRUE)
curve(f(x), -5, 5, col = "red", 
      yaxt = "n", ann = FALSE,
      xlim = c(-5, 5), ylim = c(-2, 2))

# 基底としてsin(x)を使用した場合の多項式回帰。
X <- cbind(1, sin(x), sin(2 * x), sin(3 * x))
beta <- solve(t(X) %*% X) %*% t(X) %*% y
g <- function(x) {
  beta[1] + beta[2] * sin(x) + beta[3] * sin(2 * x) + beta[4] * sin(3 * x)
}
par(new = TRUE)
curve(g(x), -5, 5, col = "blue", 
      yaxt = "n", ann = FALSE,
      xlim = c(-5, 5), ylim = c(-2, 2))

# ノイズを除去した元関数のグラフ
par(new = TRUE)
curve(ceiling(x) %% 2 * 2 - 1, -5, 5, col = "gray",
      ann = FALSE,
      xlim = c(-5, 5), ylim = c(-2, 2))
