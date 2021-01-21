# Load library
library(glmnet)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter5 e.g.51
df <- read.table("crime.txt")
X <- as.matrix(df[, 3:7])
y <- df[, 1]
cv_fit <- cv.glmnet(X, y) # 10-fold CVを行い、最適なlambdaの値を得る。
plot(cv_fit)
lambda_min <- cv_fit$lambda.min # MSEが最も小さくなるlambda値＝最適なlambda値。
print(lambda_min)
fit <- glmnet(X, y, lambda = lambda_min) # 得たlambda_minを用いて、通常のLassoを行う。
print(fit$beta)