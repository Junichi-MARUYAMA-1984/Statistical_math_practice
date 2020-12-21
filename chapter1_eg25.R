# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.25（beta_1 = 0の帰無仮説が成立する例）
N <- 100; r <- 1000
T <- NULL

for (i in 1:r) {
  x <- rnorm(N); y <- rnorm(N)
  x_bar <- mean(x); y_bar <- mean(y)
  fit <- lm(y ~ x)
  beta <- fit$coefficients
  RSS <- sum((y - fit$fitted.values)^2)
  RSE <- sqrt(RSS / (N - 1 - 1))
  B_1 <- 1 / sum((x - x_bar)^2)
  se_1 <- RSE * sqrt(B_1)
  T <- c(T, beta[2] / se_1)
}

hist(T, breaks = sqrt(r), probability = TRUE, 
     xlab = "tの値", ylab = "確率密度",
     main = "tの値のヒストグラムと理論値（赤）")
curve(dt(x, N - 2), -3, 3, type = "l", col = "red", add = TRUE)

# Chapter1 e.g.25（beta_1 = 0の帰無仮説が成立しない例）
N <- 100; r <- 1000
T <- NULL

for (i in 1:r) {
  x <- rnorm(N); y <- 0.1 * x + rnorm(N)
  x_bar <- mean(x); y_bar <- mean(y)
  fit <- lm(y ~ x)
  beta <- fit$coefficients
  RSS <- sum((y - fit$fitted.values)^2)
  RSE <- sqrt(RSS / (N - 1 - 1))
  B_1 <- 1 / sum((x - x_bar)^2)
  se_1 <- RSE * sqrt(B_1)
  T <- c(T, beta[2] / se_1)
}

hist(T, breaks = sqrt(r), probability = TRUE, 
     xlab = "tの値", ylab = "確率密度",
     main = "tの値のヒストグラムと理論値（赤）")
curve(dt(x, N - 2), -3, 3, type = "l", col = "red", add = TRUE)
