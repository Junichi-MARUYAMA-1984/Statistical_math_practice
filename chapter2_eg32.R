# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.32
# 多変数関数の例 f(x, y) = x^2 + y^2 - 1
f <- function(z) {
  z[1]^2 + z[2]^2 - 1
}
# df/dx
f_x <- function(z) {
  2 * z[1]
}
# df/dy
f_y <- function(z) {
  2 * z[2]
}
# 多変数関数の例 g(x, y) = x + y
g <- function(z) {
  z[1] + z[2]
}
# dg/dx
g_x <- function(z) {
  1
}
# dg/dy
g_y <- function(z) {
  1
}
# Newton-Raphson法の適用
z <- c(3, 4)
for (i in 1:10) {
  z <- z - solve(matrix(c(f_x(z), f_y(z), g_x(z), g_y(z)),
                        ncol = 2, byrow = TRUE)) %*% c(f(z), g(z))
}
print(z)