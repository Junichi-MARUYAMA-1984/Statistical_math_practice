# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter1 e.g.29
f <- function(x) {
  exp(beta_0 + beta * x) / (1 + exp(beta_0 + beta * x))
}

beta_0 <- 0
beta_seq <- c(0, 0.2, 0.5, 1, 2, 10)
m <- length(beta_seq)
beta <- beta_seq[1]
plot(f, xlim = c(-10, 10), ylim = c(0, 1),
     xlab = "x", ylab = "P(Y = 1 | x)",
     col = 1,
     main = "ロジスティック曲線")

for (i in 2:m) {
  beta <- beta_seq[i]
  par(new = TRUE)
  plot(f, xlim = c(-10, 10), ylim = c(0, 1),
       xlab = "", ylab = "",
       axes = FALSE,
       col = i)
}
legend("topleft", legend = beta_seq,
       col = 1:length(beta_seq), 
       lwd = 2, 
       cex = .8)
