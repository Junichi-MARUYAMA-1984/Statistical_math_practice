# Clear environment
rm(list = ls())
  
# Chapter1 e.g.24
# xもyもN(0, 1)に従う100個の乱数からなるベクトルとし、
# それらを単回帰分析するケース。
# 当然相関は無いので、回帰係数は切片も傾きも0となることが予想される。
# それを検定できるか。
N <- 100
x <- rnorm(N); y <- rnorm(N)
x_bar <- mean(x); y_bar <- mean(y)

beta_0 <- sum(y_bar * sum(x^2) - x_bar * sum(x * y)) / sum((x - x_bar)^2) # 最小二乗法による推定値beta_0（切片）
beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2) # 最小二乗法による推定値beta_1（傾き）
RSS <- sum((y - beta_0 - beta_1 * x)^2) # 残差二乗和
RSE <- sqrt(RSS / (N - 1 - 1)) # 誤差の標準偏差の不偏推定量
B_0 <- sum(x^2) / N / sum((x - x_bar)^2) # betaの分散共分散行列の第一対角成分
B_1 <- 1 / sum((x - x_bar)^2) # betaの分散共分散行列の第二対角成分
se_0 <- RSE * sqrt(B_0) # 「beta_0の平均値の標準誤差」の不偏推定量
se_1 <- RSE * sqrt(B_1) # 「beta_1の平均値の標準誤差」の不偏推定量
t_0 <- beta_0 / se_0 # 「beta_0 != 0」の検定を行うためのt統計量
t_1 <- beta_1 / se_1 # 「beta_1 != 0」の検定を行うためのt統計量
p_0 <- 2 * (1 - pt(abs(t_0), N - 2)) # beta_0のP値（pt(a, b)は、自由度bのt分布におけるaの下側確率を返す。）
p_1 <- 2 * (1 - pt(abs(t_1), N - 2)) # beta_1のP値（（説明続き）今回aは正の値なので、1-pt(a,b)として上側確率にすることで求めたい確率を計算できる。）

beta_0; se_0; t_0; p_0
beta_1; se_1; t_1; p_1

lm(y ~ x)
summary(lm(y ~ x))

# Chapter1 e.g.24_modified
# xをN(0, 1)に従う100個の乱数からなるベクトルとし、
# yをy = 1 + x + N(0, 1)で規定される100個の要素からなるベクトルとして、
# それらを単回帰分析するケース。
# y = 1 + xという相関があるので、回帰係数は切片 = 1, 傾き = 1となることが予想される。
# それらの回帰係数が0でないということを検定できるか。
N <- 100
x <- rnorm(N); y <- 1 + x + rnorm(N)
x_bar <- mean(x); y_bar <- mean(y)

beta_0 <- sum(y_bar * sum(x^2) - x_bar * sum(x * y)) / sum((x - x_bar)^2) # 最小二乗法による推定値beta_0（切片）
beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2) # 最小二乗法による推定値beta_1（傾き）
RSS <- sum((y - beta_0 - beta_1 * x)^2) # 残差二乗和
RSE <- sqrt(RSS / (N - 1 - 1)) # 誤差の標準偏差の不偏推定量
B_0 <- sum(x^2) / N / sum((x - x_bar)^2) # betaの分散共分散行列の第一対角成分
B_1 <- 1 / sum((x - x_bar)^2) # betaの分散共分散行列の第二対角成分
se_0 <- RSE * sqrt(B_0) # 「beta_0の平均値の標準誤差」の不偏推定量
se_1 <- RSE * sqrt(B_1) # 「beta_1の平均値の標準誤差」の不偏推定量
t_0 <- beta_0 / se_0 # 「beta_0 != 0」の検定を行うためのt統計量
t_1 <- beta_1 / se_1 # 「beta_1 != 0」の検定を行うためのt統計量
p_0 <- 2 * (1 - pt(abs(t_0), N - 2)) # beta_0のP値（pt(a, b)は、自由度bのt分布におけるaの下側確率を返す。）
p_1 <- 2 * (1 - pt(abs(t_1), N - 2)) # beta_1のP値（（説明続き）今回aは正の値なので、1-pt(a,b)として上側確率にすることで求めたい確率を計算できる。）

beta_0; se_0; t_0; p_0
beta_1; se_1; t_1; p_1

lm(y ~ x)
summary(lm(y ~ x))
