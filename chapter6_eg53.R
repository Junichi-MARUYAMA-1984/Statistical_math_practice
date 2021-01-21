# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.53
# 一般の多項式回帰

# 偶関数に近いデータの作成。
# このコードの場合、テキストの(6.1)式とは違う関数のグラフが出力される。
# しかしながら、こいつの場合の方がcos(x)の当てはまりが良くなっている。
# 恐らく、x = 0周辺の周期が他の部分と同じか異なるかに依るのであろう。
n <- 100
x <- rnorm(n) * pi
y <- round(x) %% 2 * 2 - 1 + rnorm(n) * 0.2
plot(x, y, 
     xaxt = "n", yaxt = "n",
     # ann = FALSE, 
     main = "偶関数の乱数をsin, cosで追従")

# 基底としてcos(x)を使用した場合の多項式回帰。
X <- cbind(1, cos(x), cos(2 * x), cos(3 * x))
beta <- solve(t(X) %*% X) %*% t(X) %*% y
f <- function(x) {
  beta[1] + beta[2] * cos(x) + beta[3] * cos(2 * x) + beta[4] * cos(3 * x)
}
par(new = TRUE)
curve(f(x), -5, 5, col = "red", yaxt = "n", ann = FALSE)

# 基底としてsin(x)を使用した場合の多項式回帰。
X <- cbind(1, sin(x), sin(2 * x), sin(3 * x))
beta <- solve(t(X) %*% X) %*% t(X) %*% y
g <- function(x) {
  beta[1] + beta[2] * sin(x) + beta[3] * sin(2 * x) + beta[4] * sin(3 * x)
}
par(new = TRUE)
curve(g(x), -5, 5, col = "blue", yaxt = "n", ann = FALSE)

# ノイズを除去した元関数のグラフ
curve(round(x) %% 2 * 2 - 1, -3, 3)
