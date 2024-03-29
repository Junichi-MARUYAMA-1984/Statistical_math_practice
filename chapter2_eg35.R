# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.35
# 人工的に「赤」群と「青」群のデータ群をそれぞれ100個作成する。
# 今回のシミュレーションでは、
# 赤群データが応答変数y = -1のもとで分布する説明変数x、
# 青群データが応答変数y = +1のもとで分布する説明変数xであるとする。
# 今回、説明変数xは二個の要素を有する二次元ベクトルであるとする。
# 
# u: N(0, 1)に従う乱数。
# v: N(0, 1)に従う乱数。uとは独立。
# として、
# 赤群：
# 説明変数xの要素としてx_1, y_1を取るとする。
# 座標(x_1, y_1) = (2, 2)を中心に、
# x_1 = u * 2 + 2
# y_1 = v * 2 + 2
# という風に、円形に散らばせたデータ。
# 青群：
# 説明変数xの要素としてx_2, y_2を取るとする。
# 座標(x_2, y_2) = (-3, -3)を中心に、
# x_2 = u - 3
# y_2 = (-0.8 * u + sqrt(1 - (0.8^2)) * v) - 3
# という風に、y = -0.8*(x + 3) - 3の直線に従って散らばせたデータ。
# 赤群と青群でuとvの値は異なる。
n <- 100
# 赤群データの生成
mu_1 <- c(2, 2)
sigma_1 <- 2
sigma_2 <- 2
rho_1 <- 0
u <- rnorm(n)
v <- rnorm(n)
x_1 <- sigma_1 * u + mu_1[1]
y_1 <- (rho_1 * u + sqrt(1 - rho_1 ^ 2) * v) * sigma_2 + mu_1[2]

# 青群データの生成
mu_2 <- c(-3, -3)
sigma_3 <- 1
sigma_4 <- 1
rho_2 <- -0.8
u <- rnorm(n)
v <- rnorm(n)
x_2 <- sigma_3 * u + mu_2[1]
y_2 <- (rho_2 * u + sqrt(1 - rho_2 ^ 2) * v) * sigma_4 + mu_2[2]

# y = +-1のもとでのxの確率密度関数（多次元正規分布）
# f_+-x(x) = {(2pi)^p * det(cov(x))}^(-1/2) * exp{(-1/2) * t(x - mu_+-1) %*% solve(cov(x)) %*% (x - mu_+-1)}
# において、
# 係数{(2pi)^p}^(-1/2)を無視し、両辺のlogを取った式の右辺を、
# 以下のコードで関数fとして定義した。
# この先の処理で知りたいのは赤群と青群の事後確率の境界線なので、
# このように変形した関数を用いて構わない。
f <- function(x, mu, inv, de) {
  # drop関数は、サイズ1 x 1のarray変数値をただのスカラー値に変換する。
  drop(-0.5 * t(x - mu) %*% inv %*% (x - mu) - 0.5 * log(de))
}

# 赤群データの分布を推定。
# データがN(mu_1, mat)という二次元正規分布に従うものと仮定している。
mu_1 <- mean(c(x_1, y_1)) # 赤群データの推定平均値
df_1 <- data.frame(x_1, y_1)
mat_1 <- cov(df_1) # 赤群データの分散共分散行列
inv_1 <- solve(mat_1) # 分散共分散行列の逆行列
de_1 <- det(mat_1) # 分散共分散行列の行列式
f_1 <- function(u, v) {
  f(c(u, v), mu_1, inv_1, de_1)
}

# 青群データの分布を推定
# データがN(mu_2, mat)という二次元正規分布に従うものと仮定している。
mu_2 <- mean(c(x_2, y_2)) # 青群データの推定平均値
df_2 <- data.frame(x_2, y_2)
mat_2 <- cov(df_2) # 青群データの分散共分散行列
inv_2 <- solve(mat_2) # 分散共分散行列の逆行列
de_2 <- det(mat_2) # 分散共分散行列の行列式
f_2 <- function(u, v) {
  f(c(u, v), mu_2, inv_2, de_2)
}

# y = +1とy = -1の事後確率分布の境界線を求める。
# 
# 応答変数y = -1, y = +1の事前確率は、それぞれpi_1, pi_2であることが分かっているとする。
# すると、y = +1, y = -1の事後確率の境界線は、
# log(pi_1) * f_1 = log(pi_2) * f_2
# が表す曲線となる。
# （f_1, f_2は、上で定義したように、
# 既にlogを取った確率密度関数であることに注意！）
# 以下のコードでは、
# w = log(pi_1) * f_1 - log(pi_2) * f_2
# として、
# w = 0となる等高線をx-y平面上にプロットしている。
# この場合のx-y平面は、当然、
# 説明変数xが持つ二個の要素をそれぞれx座標値、y座標値としてプロットした平面である。
pi_1 <- 0.5 # y = -1の事前確率
pi_2 <- 0.5 # y = +1の事前確率
u <- v <- seq(-6, 6, length = 50)
m <- length(u)
w <- array(dim = c(m, m))
for (i in 1:m) {
  for (j in 1:m) {
    w[i, j] <- log(pi_1) + f_1(u[i], v[j]) - log(pi_2) - f_2(u[i], v[j])
  }
}
contour(u, v, w, level = 0) # w = 0の等高線を描画。
points(x_1, y_1, col = "red") # 赤群データを描画。
points(x_2, y_2, col = "blue") # 青群データを描画。
