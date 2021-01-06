# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.38
# ある検査の測定値が、
# 正常者ではN(-1, 1)に、病気者ではN(1, 1)に従うものとする。
# 実際の確率密度関数としてf_0, f_1を
# f_0(x) = N(-1, 1), f_1(x) = N(1, 1)としたときに、
# ある被験者iの測定値x_iに対して
# f_1(x_i)/f_0(x_i) >= theta
# となった時に、被験者を病気と診断することにする。
# ただしthetaは診断基準値であり、正の値を自由に取るものとする。
N_0 <- 10000 # 正常の人の人数
N_1 <- 10 # 病気の人の人数
mu_0 <- -1 # 正常下での検査平均値
mu_1 <- 1 # 病気下での検査平均値
var_0 <- 1 # 正常下での検査値標準偏差
var_1 <- 1 # 病気下での検査値標準偏差
x <- rnorm(N_0, mu_0, var_0) # 正常下での分布に従う測定値を生成
y <- rnorm(N_1, mu_1, var_1) # 病気下での分布に従う測定値を生成

theta_seq <- exp(seq(-10, 100, 0.5)) # 診断基準値を生成
U <- NULL # 各診断基準値における「正常者を病気と誤診する確率」
V <- NULL # 各診断基準値における「有病者を病気と正しく診断する確率」

# pnormバージョン（私はこちらは間違っていると思う）
# pnorm()は第一引数に対する累積確率（下側確率）を返すので、
# テキスト中で意図しているような計算にならない気がします。
# True positiveをoverestimateしてしまうと思う。
plot(1:1, 1:1, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive",
     ylab = "True Positive",
     main = "ROC曲線(pnorm)",
     type = "n")
for (theta in theta_seq) {
  u <- sum(pnorm(x, mu_1, var_1) / pnorm(x, mu_0, var_0) > theta) / N_0 # 正常者を誤診する確率の計算
  v <- sum(pnorm(y, mu_1, var_1) / pnorm(y, mu_0, var_0) > theta) / N_1 # 有病者を正しく診断する確率の計算
  U <- c(U, u) # 計算結果を配列に追記
  V <- c(V, v) # 計算結果を配列に追記
}
lines(U, V, col = "lightgray")
points(U, V, col = "blue")
M <- length(theta_seq) - 1
AUC <- 0 
for (i in 1:M) {
  AUC <- AUC + abs(U[i + 1] - U[i]) * V[i] # 区分求積法によりAUCを計算
}
text(0.5, 0.5, paste("AUC = ", AUC), col = "red")

# dnormバージョン（私はこちらが正しいと思う）
# dnorm()は第一引数に対する確率密度関数値（関数のyの値）を返す。
# こちらの方が、テキスト中で意図している計算だと思う。
plot(1:1, 1:1, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive",
     ylab = "True Positive",
     main = "ROC曲線(dnorm)",
     type = "n")
U <- NULL
V <- NULL
for (theta in theta_seq) {
  u <- sum(dnorm(x, mu_1, var_1) / dnorm(x, mu_0, var_0) > theta) / N_0 # 正常者を誤診する確率の計算
  v <- sum(dnorm(y, mu_1, var_1) / dnorm(y, mu_0, var_0) > theta) / N_1 # 有病者を正しく診断する確率の計算
  U <- c(U, u) # 計算結果を配列に追記
  V <- c(V, v) # 計算結果を配列に追記
}
lines(U, V, col = "lightgray")
points(U, V, col = "blue")
M <- length(theta_seq) - 1
AUC <- 0
for (i in 1:M) {
  AUC <- AUC + abs(U[i + 1] - U[i]) * V[i] # 区分求積法によりAUCを計算
}
text(0.5, 0.5, paste("AUC = ", AUC), col = "red")