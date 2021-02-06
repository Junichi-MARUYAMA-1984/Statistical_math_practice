# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.87
# 主成分分析（直交する主成分ベクトルの描画）
n <- 100
a <- 0.7
b <- sqrt(1 - a ^ 2)
u <- rnorm(n)
v <- rnorm(n)
x <- u
y <- u * a + v * b
plot(x, y)
plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4))
T <- prcomp(cbind(x, y))$rotation
print(T[2, 1] / T[1, 1] * T[2, 2] / T[1, 2])
abline(0, T[2, 1] / T[1, 1], col = "red")
abline(0, T[2, 2] / T[1, 2], col = "blue")