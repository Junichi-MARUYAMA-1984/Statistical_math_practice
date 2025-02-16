# Load library
library(e1071)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter8 e.g.78
# e1071パッケージを用いたサポートベクトルマシン処理
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:150, ] <- x[1:150, ] + 2 # テキストは1:100となっているが、これは不適切
x[151:200, ] <- x[151:200, ] - 2 # テキストは101:150となっているが、これは不適切
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train, ], 
              kernel = "radial",
              gamma = 1, const = 100)
plot(svmfit, dat[train, ])

tune_out <- tune(svm, y ~ ., 
                 data = dat[train, ],
                 kernel = "radial",
                 ranges = list(const = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune_out)
