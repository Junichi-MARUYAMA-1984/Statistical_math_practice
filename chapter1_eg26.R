# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.26
r2 <- function(x, y) {
  y_hat <- lm(y ~ x)$fitted.values
  y_bar <- mean(y)
  RSS <- sum((y - y_hat)^2)
  TSS <- sum((y - y_bar)^2)
  return(1 - RSS / TSS)
}

N <- 100
m <- 2
x <- matrix(rnorm(m * N), ncol = m)
y <- rnorm(N)
r2(x, y)

N <- 100
m <- 1
x <- matrix(rnorm(m * N), ncol = m)
y <- rnorm(N)
r2(x, y)
cor(x, y)^2