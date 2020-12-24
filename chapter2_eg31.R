# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.31
# 例としての関数f(x) = x^2 - 1
f <- function(x) {
  x^2 - 1
}
# f(x)の一次導関数f'(x) = 2 * x
f. <- function(x) {
  2 * x
}
# Newton-Raphson法の適用
curve(f(x), -1, 5); abline(h = 0, col = "blue")
x <- 4
for (i in 1:10) {
  X <- x
  Y <- f(x)
  x <- x - f(x) / f.(x)
  segments(X, Y, x, 0); segments(X, Y, X, 0, lty = 3)
  points(x, 0, col = "red", pch = 16)
}
