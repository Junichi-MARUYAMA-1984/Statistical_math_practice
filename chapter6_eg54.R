# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.54
# スプライン回帰の実装。
# sin(x)に近いデータセットを生成し、K = 5, 7, 9のスプライン回帰を行う。
n <- 100
x <- rnorm(n) * 2 * pi
y <- sin(x) + 0.2 * rnorm(n)
col_set <- c("red", "green", "blue")
K_set <- c(5, 7, 9) # 数直線空間の区切り数

# スプライン回帰処理
for (k in 1:3) {
  K <- K_set[k]
  knots <- seq(-2 * pi, 2 * pi, length = K) # 区切り点の設定
  X <- matrix(nrow = n, ncol = K + 4)
  for (i in 1:n) {
    X[i, 1] <- 1
    X[i, 2] <- x[i]
    X[i, 3] <- x[i] ^ 2
    X[i, 4] <- x[i] ^ 3
    for (j in 1:K) {
      X[i, j + 4] <- max((x[i] - knots[j]) ^ 3, 0) # 説明変数行列Xの計算の実装に注目。
    }
  }
  beta <- solve(t(X) %*% X) %*% t(X) %*% y # 最小二乗推定値の計算
  f <- function(x) { # スプライン曲線描画用関数
    S <- beta[1] + beta[2] * x + beta[3] * x ^ 2 + beta[4] * x ^ 3
    for (j in 1:K) {
      S <- S + beta[j + 4] * max((x - knots[j]) ^ 3, 0) # ここの実装も賢い。
    }
    return(S)
  }
  u_seq <- seq(-5, 5, 0.02) # スプライン曲線描画用x値
  v_seq <- NULL # スプライン曲線描画用y値
  for (u in u_seq) {
    v_seq <- c(v_seq, f(u))
  }
  plot(u_seq, v_seq, 
       type = "l", 
       col = col_set[k],
       yaxt = "n", 
       xlab = "x", ylab = "f(x)")
  par(new = TRUE)
}
points(x, y)
legend(-2.2, 1, 
       paste0("K = ", K_set),
       lty = 1,
       col = col_set)