# Clear environment
rm(list = ls())

# Load libraries
library(MASS)

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.27
r2 <- function(x, y) {
  y_hat <- lm(y ~ x)$fitted.values
  y_bar <- mean(y)
  RSS <- sum((y - y_hat)^2)
  TSS <- sum((y - y_bar)^2)
  return(1 - RSS / TSS)
}

vif <- function(x) {
  p <- ncol(x)
  values <- array(dim = p)
  for (j in 1:p) {
    values[j] <- 1 / (1 - r2(x[, -j], x[, j]))
  }
  return(values)
}

x <- as.matrix(Boston)
vif(x)