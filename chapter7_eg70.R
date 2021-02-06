# Load library
library(MASS)
library(gbm)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter7 e.g.70
# gbmパッケージを用いたブースティング処理
train <- 1:200
test <- 201:300
boston_test <- Boston[test, 14]
MAX <- 5000
nn <- c(seq(1, 9, 1), seq(10, 90, 10), seq(100, MAX, 50))
plot(nn, nn / MAX * 80, type = "n",
     xlab = "生成した木の個数", ylab = "テストデータでの二乗誤差")
d <- 1:3
color <- c("blue", "green", "red")
for (i in 1:3) {
  x <- NULL; y <- NULL
  for (n in nn) {
    boost_boston <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = n,
                        interaction.depth = i,
                        shrinkage = 0.001)
    yhat_boost <- predict(boost_boston, n.trees = n,
                          newdata = Boston[test, ])
    S <- mean((yhat_boost - boston_test) ^ 2)
    x <- c(x, n); y <- c(y, S)
  }
  lines(x, y, col = color[i])
}
legend("topright", legend = c("d = 1", "d = 2", "d = 3"),
       col = color, lwd = 2, cex = 0.8)