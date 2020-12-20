# Clear environment
rm(list = ls())
  
# Chapter1 e.g.23
n <- 100; x <- rnorm(n) + 2
plot(1, 1, 
     xlim = c(0.5, 1.5),
     ylim = c(0.5, 1.5),
     xlab = "beta_0",
     ylab = "beta_1")
for (i in 1:100) {
  y <- 1 + x + rnorm(n)
  z <- cbind(1, x)
  beta_est <- solve(t(z) %*% z) %*% t(z) %*% y
  points(beta_est[1], beta_est[2], col = i)
}
abline(v = 1); abline(h = 1)
sum(x) / n
sum(x^2) / n

# Chapter1 e.g.23_modified
n <- 100; x <- rnorm(n)
plot(2, 1, 
     xlim = c(1.5, 2.5),
     ylim = c(0.5, 1.5),
     xlab = "beta_0",
     ylab = "beta_1")
for (i in 1:100) {
  y <- 2 + x + rnorm(n)
  z <- cbind(1, x)
  beta_est <- solve(t(z) %*% z) %*% t(z) %*% y
  points(beta_est[1], beta_est[2], col = i)
}
abline(v = 2); abline(h = 1)
sum(x) / n
sum(x^2) / n