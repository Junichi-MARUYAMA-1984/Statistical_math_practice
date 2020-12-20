# Clear environment
rm(list = ls())

# Chapter1 e.g.19
# 最小二乗法の切片と傾きを求める関数min_sq
min_sq <- function(x, y) {
  x_bar <- mean(x)
  y_bar <- mean(y)
  beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
  beta_0 <- y_bar - beta_1 * x_bar
  return(list(a = beta_0, b = beta_1))
}

a <- rnorm(1); b <- rnorm(1) # 直線の係数をランダムに生成
N <- 100
x <- rnorm(N); y <- a * x + b + rnorm(N) # 直線の周りの点をランダムに生成
plot(x, y); abline(h = 0); abline(v = 0) # 点のプロット
min_sq_bf <- min_sq(x, y)
abline(min_sq_bf$a, min_sq_bf$b, col = "red") #中心化前の直線
x <- x - mean(x); y <- y - mean(y) # 中心化
min_sq_af <- min_sq(x, y)
abline(min_sq_af$a, min_sq_af$b, col = "blue") # 中心化後の直線
legend("topleft", c("中心化前", "中心化後"),
       lty = 1,
       col = c("red", "blue")) # 凡例