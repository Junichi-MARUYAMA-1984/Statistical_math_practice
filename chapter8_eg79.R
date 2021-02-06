# Load library
library(e1071)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter8 e.g.79
# 三水準以上の場合のサポートベクトルマシン処理
df <- iris
x <- df[, 1:4]
y <- as.factor(df[, 5])
m <- 30
train <- sample(1:150, 150 - m)
x_train <- x[train, ]
y_train <- y[train]
dat <- data.frame(x_train, y_train)
svmfit <- svm(y_train ~ ., data = dat,
              kernel = "radial", const = 10, gamma = 1)
x_test <- x[-train, ]
y_test <- y[-train]
table(y_test, predict(svmfit, x_test))