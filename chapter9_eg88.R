# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.88
# 主成分分析（多次元データの第1/第2主成分への射影）
pr_out <- prcomp(USArrests, scale = TRUE)
biplot(pr_out)
pr_out$x <- -pr_out$x
pr_out$rotation <- -pr_out$rotation
biplot(pr_out)