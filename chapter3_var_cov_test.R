# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 varとcovvの実装（鈴木テキスト中の式は誤りでは？）
var_maru <- function(x) {
  n <- length(x)
  if (n <= 1) {
    warning("Error: The number of sample is inappropreate.")
    return(-1)
  }
  term1 <- drop(t(x) %*% x)
  term2 <- (sum(x)) ^ 2 / n
  return((term1 - term2) / (n - 1))
}

cov_maru <- function(x, y) {
  n <- length(x)
  if (n <= 1 || n != length(y)) {
    warning("Error: The number of sample is inappropreate.")
    return(-1)
  }
  term1 <- drop(t(x) %*% y)
  term2 <- (sum(x) * sum(y)) / n
  return((term1 - term2) / (n - 1))
}

var_suzuki <- function(x) {
  n <- length(x)
  if (n <= 1) {
    warning("Error: The number of sample is inappropreate.")
    return(-1)
  }
  term1 <- drop(t(x) %*% x)
  term2 <- n * (sum(x)) ^ 2
  return((term1 - term2) / (n - 1))
}

cov_suzuki <- function(x, y) {
  n <- length(x)
  if (n <= 1 || n != length(y)) {
    warning("Error: The number of sample is inappropreate.")
    return(-1)
  }
  term1 <- drop(t(x) %*% y)
  term2 <- n * (sum(x) * sum(y))
  return((term1 - term2) / (n - 1))
}

# テスト
x <- 1:20
y <- seq(21, 60, 2)
var(x)
var_maru(x)
var_suzuki(x)
cov(x, y)
cov_maru(x, y)
cov_suzuki(x, y)