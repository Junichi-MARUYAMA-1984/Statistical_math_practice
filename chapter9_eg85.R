# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.85
# 階層的クラスタリング
# 樹形図の作成（hclust()の利用）
n <- 100
x <- matrix(rnorm(n * 2), ncol = 2)
par(mfrow = c(2, 2))
hc_complete <- hclust(dist(x), method = "complete")
plot(hc_complete)
hc_single <- hclust(dist(x), method = "single")
plot(hc_single)
hc_centroid <- hclust(dist(x), method = "centroid")
plot(hc_centroid)
hc_average <- hclust(dist(x), method = "average")
plot(hc_average)