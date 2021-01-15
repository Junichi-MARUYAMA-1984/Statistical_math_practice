# Clear environment
rm(list = ls())

# Chapter1 e.g.21
i <- 1
curve(dchisq(x, i), 0, 20, col = i)
for(i in 2:10) {
  curve(dchisq(x, i), 0, 20, col = i, add = TRUE, ann = FALSE)
  legend("topright", legend = 1:10, lty = 1, col = 1:10)
}