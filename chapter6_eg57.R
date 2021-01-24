# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.57
# 平滑化スプラインの実装

# 命題20で定義される関数d_j(x)
d <- function(j, x, knots) {
  K <- length(knots)
  return((max((x - knots[j]) ^ 3, 0) - max((x - knots[K]) ^ 3, 0)) / (knots[K] - knots[j]))
}
# 命題20で定義される関数h_j(x)
h <- function(j, x, knots) {
  K <- length(knots)
  if (j == 1) {
    return(1)
  } else if (j == 2) {
    return(x)
  } else {
    return(d(j - 2, x, knots) - d(K - 1, x, knots))
  }
}

# 命題22で与えられる、式(6.6)のg_i,jの具体形
# 引数xの要素は、昇順ソートされていることを前提とする。
G <- function(x) {
  n <- length(x)
  g <- matrix(0, nrow = n, ncol = n)
  for (i in 3:n) {
    for (j in i:n) {
      denom <- (x[n] - x[i - 2]) * (x[n] - x[j - 2]) # 分母を先に計算。ゼロ除算の対応のため。
      if (denom != 0) {
        g[i, j] <- 12 * (x[n] - x[n - 1]) * (x[n - 1] - x[j - 2]) * (x[n - 1] - x[i - 2]) / denom +
          (12 * x[n - 1] + 6 * x[j - 2] - 18 * x[i - 2]) * (x[n - 1] - x[j - 2]) ^ 2 / denom
        g[j, i] <- g[i, j]
      } else {
        stop("Zero division")
        return(-1)
      }
    }
  }
  return(g)
}

# 平滑化スプライン処理のメインルーチン
# lambda = 1, 30, 80で振って、スプライン曲線の形状を比較する。
n <- 100
x <- runif(n, -5, 5)
y <- x + 2 * sin(x) + rnorm(n)
index <- order(x) # 昇順ソートのための添字列を取得
x <- x[index]; y <- y[index] # 昇順ソート
X <- matrix(nrow = n, ncol = n) # 自然なスプライン回帰の時と同じ定義の行列X
X[, 1] <- 1 # 行列Xの第1列の要素は1
for (j in 2:n) {
  for (i in 1:n) {
    X[i, j] <- h(j, x[i], x) # 自然なスプライン回帰と同様にXを計算。
  }
}
GG <- G(x) # i,j成分が式(6.6)のg_i,jで定義される行列GG
lambda_set <- c(1, 30, 80) # 平滑化スプラインのパラメータであるlambda
col_set <- c("red", "blue", "green")
for (i in 1:3) {
  lambda <- lambda_set[i]
  gamma <- solve(t(X) %*% X + lambda * GG) %*% t(X) %*% y # 最小二乗推定値の計算
  g <- function(u) { # スプライン曲線描画用関数
    S <- gamma[1]
    for (j in 2:n) {
      S <- S + gamma[j] * h(j, u, x)
    }
    return(S)
  }
  u_seq <- seq(-8, 8, 0.02) # スプライン曲線描画用x値
  v_seq <- NULL # スプライン曲線描画用y値
  for (u in u_seq) {
    v_seq <- c(v_seq, g(u)) # スプライン曲線描画用座標値の計算
  }
  plot(u_seq, v_seq,
       type = "l", yaxt = "n", 
       xlab = "x",
       ylab = "g(x)",
       ylim = c(-8, 8),
       col = col_set[i])
  par(new = TRUE)
}
points(x, y)
legend("topleft", paste0("lambda = ", lambda_set),
       col = col_set,
       lty = 1)
title("平滑化スプライン　(n = 100)")