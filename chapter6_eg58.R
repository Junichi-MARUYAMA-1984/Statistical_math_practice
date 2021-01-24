# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.58
# CVによる平滑化スプラインパラメータlambda/有効自由度と予測誤差の評価

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

# クロスバリデーションによる平滑化スプライン予測誤差の評価。
# lambdaが大きくなると、ハット行列H[lambda]のトレースの値が小さくなる。
# このH[lambda]のトレース値を有効自由度と呼ぶ。
# cv_ss_fast()は、引数lambdaに対する有効自由度とCV予測誤差を返す。
cv_ss_fast <- function(X, y, lambda, G, k) {
  n <- length(y)
  m <- n / k # m-fold CVを行う。
  H <- X %*% solve(t(X) %*% X + lambda * G) %*% t(X) # ハット行列H[lambda]の計算
  df <- sum(diag(H)) # 有効自由度 = tr(H[lambda])
  I_n <- diag(rep(1, n)) # n x nの単位行列
  e <- (I_n - H) %*% y # 誤差ベクトル（= y - yhat = y - X %*% gamma）
  I_m <- diag(rep(1, m)) # m x mの単位行列
  S <- 0 # CV誤差の合計
  for (j in 1:k) {
    test <- ((j - 1) * m + 1) : (j * m)
    S <- S + norm(solve(I_m - H[test, test]) %*% e[test], "2") ^ 2 # CV処理
  }
  return(list(score = S / n, df = df)) # CV予測誤差（S/n）と有効自由度を返す。
}

# 平滑化スプライン回帰を行い、有効自由度とCV予測誤差の関係を見てみる。
n <- 100
x <- runif(n, -5, 5)
y <- x + 2 * sin(x) + 3 * rnorm(n) # テキストの式はほぼ直線だったので、より平滑化スプラインに相応しいデータにした。
index <- order(x) # 平滑化スプラインなので、サンプルはxで昇順ソートしておく。
x <- x[index]; y <- y[index] # xの昇順ソート
X <- matrix(nrow = n, ncol = n) # yの昇順ソート
X[, 1] <- 1
for (j in 2:n) {
  for (i in 1:n) {
    X[i, j] <- h(j, x[i], x) # 自然なスプライン回帰における行列Xの計算。
  }
}
GG <- G(x) # (6.6)式のg_i,jを(i,j)成分に持つ行列GG
u <- seq(1, 50) # CVで評価するlambdaの範囲
v <- NULL
w <- NULL
for (lambda in u) {
  result <- cv_ss_fast(X, y, lambda, GG, n) # CVで予測誤差を評価。
  v <- c(v, result$df) # 有効自由度
  w <- c(w, result$score) # CV予測誤差
}
plot(v, w, 
     type = "l", col = "red",
     xlab = "有効自由度", ylab = "CVによる予測誤差")
title("有効自由度とCVによる予測誤差")
par(new = FALSE)
plot(u, w, 
     type = "l", col = "blue",
     xlab = "lambda", ylab = "CVによる予測誤差")
title("lambdaとCVによる予測誤差")