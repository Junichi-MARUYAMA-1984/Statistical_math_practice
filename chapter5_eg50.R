# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter5 e.g.50
# Lassoの実装。
# 今回の実装では、正規化した説明変数値に対するbetaを計算しており、
# 正規化前の説明変数値にbetaを適合させる処理は行っていない。
# 本来なら、最後の処理としてbeta値を正規化前説明変数の標準偏差で割る必要がある。
soft_th <- function(lambda, x) { # テキスト中のS_lambda(x)関数の実装。
  sign(x) * pmax(abs(x) - lambda, 0) # sign()は、引数の符号を返す。pmax()は、2つの引数のうち（符号も加味して）大きい方を返す。
}
lasso <- function(X, y, lambda = 0) {
  X <- as.matrix(X) # データフレーム型を行列型に変換。
  X <- scale(X) # 行列Xの各列を正規化。（平均0、分散1に変換）
  p <- ncol(X)
  n <- length(y)
  X_bar <- array(dim = p)
  for (j in 1:p) {
    X_bar[j] <- mean(X[, j])
    X[, j] <- X[, j] - X_bar[j] # 行列Xの各列よりそれぞれの平均値を引いているが、Xは正規化済みなのでここでは意味のない処理。
  }
  y_bar <- mean(y)
  y <- y - y_bar # 応答変数yを中心化。
  eps <- 1 # 旧betaと新betaの差値（Lassoの収束判定に用いる）
  beta <- array(0, dim = p) # 新beta
  beta_old <- array(0, dim = p) # 旧beta
  while (eps > 0.001) { # betaの更新前後の差が0.001を切ったら終了
    for (j in 1:p) {
      r <- y - X[, -j] %*% beta[-j] # テキスト中のr_i,jに相当する変数
      beta[j] <- soft_th(lambda, sum(r * X[, j]) / n) # Lasso処理
    }
    eps <- max(abs(beta - beta_old))
    beta_old <- beta
  }
  beta_0 <- y_bar - sum(X_bar * beta)
  return(list(beta = beta, beta_0 = beta_0))
}

# Crimeデータを用いてLassoを実行。
df <- read.table("crime.txt")
x <- df[, 3:7]
y <- df[, 1]
p <- ncol(x)
lambda_seq <- seq(0, 100, 0.1)
coef_seq <- lambda_seq
plot(lambda_seq, coef_seq,
     xlim = c(0, 100), ylim = c(-40, 40),
     xlab = "lambda", ylab = "beta",
     main = "各lambdaについての各係数の値",
     type = "n",
     col = "red")
for (j in 1:p) {
  coef_seq <- NULL
  for (lambda in lambda_seq) {
    coef_seq <- c(coef_seq, lasso(x, y, lambda)$beta[j])
  }
  par(new = TRUE)
  lines(lambda_seq, coef_seq, col = j)
}
legend("topright", 
       legend = c("警察への年間資金", 
                  "25歳以上で高校を卒業した人の割合",
                  "16～19歳で高校に通っていない人の割合", 
                  "18～24歳で大学生の割合",
                  "25歳以上で4年制大学を卒業した人の割合"),
       col = 1:p,
       lwd = 2,
       cex = 0.8)
