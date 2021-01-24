# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter6 e.g.60
n <- 250
x <- 2 * rnorm(n)
y <- sin(2 * pi * x) + rnorm(n) / 4

D <- function(t) {
  return(max(0.75 * (1 - t ^ 2), 0))
}

K <- function(x, y, lambda) {
  return(D(abs(x - y) / lambda))
}

f <- function(z, lambda) {
  numer <- 0
  denom <- 0
  for (i in 1:n) {
    numer <- numer + K(x[i], z, lambda) * y[i]
    denom <- denom + K(x[i], z, lambda)
  }
  return(numer / denom)
}

plot(seq(-3, 3, length = 10), seq(-2, 3, length = 10),
     type = "n",
     xlab = "x", ylab = "y")
points(x, y)
xx <- seq(-3, 3, 0.1)
yy <- NULL
for (zz in xx) {
  yy <- c(yy, f(zz, 0.05))
}
lines(xx, yy, col = "green")
yy <- NULL
for (zz in xx) {
  yy <- c(yy, f(zz, 0.25))
}
lines(xx, yy, col = "blue")

m <- n / 10
lambda_seq <- seq(0.05, 1, 0.01)
SS_min <- Inf
for (lambda in lambda_seq) {
  SS <- 0
  for (k in 1:10) {
    test <- ((k - 1) * m + 1): (k * m)
    train <- setdiff(1:n, test)
    for (j in test) {
      u <- 0
      v <- 0
      for (i in train) {
        kk <- K(x[i], x[j], lambda)
        u <- u + kk * y[i]
        v <- v + kk
      }
      if (v == 0) {
        d_min <- Inf
        for (i in train) {
          d <- abs(x[j] - x[i])
          if (d < d_min) {
            d_min <- d
            index <- i
          }
        }
        z <- y[index]
      } else {
        z <- u / v
      }
      SS <- SS + (y[j] - z) ^ 2
    }
  }
  if (SS < SS_min) {
    SS_min <- SS
    lambda_best <- lambda
  }
}

yy <- NULL
for (zz in xx) {
  yy <- c(yy, f(zz, lambda_best))
}
lines(xx, yy, col = "red")
title("Nadaraya-Watson推定量")
legend("topleft", 
       legend = paste0("lambda = ", c(0.05, 0.25, "lambda_best")),
       lwd = 1,
       col = c("green", "blue", "red"))