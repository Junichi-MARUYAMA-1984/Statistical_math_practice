# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.28
# 観測データ生成
N <- 100 # データ数
p <- 1 # 説明変数は一個で単回帰とする
X <- matrix(rnorm(N * p), ncol = p); X <- cbind(rep(1, N), X) # 説明変数行列の作成
beta <- c(1, 1) # yとXを関係付ける真のパラメタ。線形回帰によりこの値を推定する。
epsilon <- rnorm(N) # 誤差 ~ N(0, 1)
y <- X %*% beta + epsilon # パラメタbetaによりXからyを生成。
                          # ここで得られた(X, y)が今回の線形回帰に用いる観測データとなる。

# 信頼区間または予測区間を返す関数f(x)を定義。
U <- solve(t(X) %*% X) # t(X) %*% Xの逆行列
beta_hat <- U %*% t(X) %*% y # 回帰係数。すなわちパラメタbetaの推定値。
RSS <- sum((y - X %*% beta_hat)^2) # 残差二乗和
RSE <- sqrt(RSS / (N - p - 1)) # beta_hatの平均値の標準誤差。すなわちパラメタbeta推定値の標準誤差。
alpha <- 0.05 # 危険率。信頼・予測区間は(1 - alpha) x 100%信頼・予測区間となる。
f <- function(x, a) { # a = 0なら信頼区間。a = 1なら予測区間。
  x <- cbind(1, x)
  range <- qt(df = N - p - 1, 1 - alpha / 2) * RSE * sqrt(a + x %*% U %*% t(x))
  return(list(lower = x %*% beta_hat - range,
              upper = x %*% beta_hat + range))
}

x_seq <- seq(-10, 10, 0.1)

# グラフで信頼区間を表示
lower_seq <- NULL
upper_seq <- NULL
for (x in x_seq) {
  ci_ul <- f(x, 0) # 信頼区間値の取得
  lower_seq <- c(lower_seq, ci_ul$lower)
  upper_seq <- c(upper_seq, ci_ul$upper)
}
x_lim <- c(min(x_seq), max(x_seq))
y_lim <- c(min(lower_seq), max(upper_seq))
plot(x_seq, lower_seq, col = "blue", # 信頼区間下限を青線で描画
     xlim = x_lim,
     ylim = y_lim,
     xlab = "x",
     ylab = "y",
     type = "l")
par(new = TRUE)
plot(x_seq, upper_seq, col = "red", # 信頼区間上限を赤線で描画
     xlim = x_lim,
     ylim = y_lim,
     xlab = "",
     ylab = "",
     type = "l",
     axes = FALSE)
par(new = TRUE)

# グラフで予測区間を表示
lower_seq <- NULL
upper_seq <- NULL
for (x in x_seq) {
  pi_ul <- f(x, 1) # 予測区間値の取得
  lower_seq <- c(lower_seq, pi_ul$lower)
  upper_seq <- c(upper_seq, pi_ul$upper)
}
x_lim <- c(min(x_seq), max(x_seq))
y_lim <- c(min(lower_seq), max(upper_seq))
plot(x_seq, lower_seq, col = "blue", # 予測区間下限を青破線で描画
     xlim = x_lim,
     ylim = y_lim,
     xlab = "x",
     ylab = "y",
     type = "l",
     lty = 4,
     axes = FALSE)
par(new = TRUE)
plot(x_seq, upper_seq, col = "red", # 予測区間上限を赤破線で描画
     xlim = x_lim,
     ylim = y_lim,
     xlab = "",
     ylab = "",
     type = "l",
     lty = 4,
     axes = FALSE)
par(new = TRUE)
abline(beta_hat[1], beta_hat[2])
