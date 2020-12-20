  # Clear environment
  rm(list = ls())
  
  # Chapter1 e.g.22
  curve(dnorm(x), -10, 10, ann = FALSE, ylim = c(0, 0.5), lwd = 5)
  for (i in 1:10) {
    curve(dt(x, df = i), -10, 10, col = i, add = TRUE, ann = FALSE)
    legend("topright", legend = 1:10, lty = 1, col = 1:10)
  }