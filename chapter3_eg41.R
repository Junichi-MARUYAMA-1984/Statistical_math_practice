# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter3 e.g.41
# k近傍法（k-nearest neighbor algorithm）の実装
knn.1 <- function(x, y, z, k) {
  x <- as.matrix(x) # 訓練データにおける説明変数x
  n <- nrow(x) # 訓練データのデータ数
  dis <- array(dim = n) # 新しいデータzと訓練データのユークリッド距離
  for (i in 1:n) {
    dis[i] <- norm(z - x[i,], "2") # ユークリッド距離（L2ノルム）を計算。
  }
  s <- order(dis)[1:k] # 距離の小さい順にdisを並べ、上位k個を取得。
  u <- sort(table(y[s]), decreasing = TRUE) # 上位k個データにおけるy値の頻度を取得。
  
  # タイとなってしまい最頻y値が求められない場合は、
  # kの値を小さくして処理をやり直す。
  while (length(u) > 1 && u[1] == u[2]) {
    k <- k - 1
    s <- order(dis)[1:k]
    u <- sort(table(y[s]), decreasing = TRUE)
  }
  
  return(names(u)[1]) # 近傍データk個中において最も頻度の高いy値を返す。
}

# 新規データが複数の場合、それぞれのデータにおいてknn.1()を実行する。
knn <- function(x, y, z, k) {
  n <- nrow(z)
  w <- array(dim = n)
  for (i in 1:n) {
    w[i] <- knn.1(x, y, z[i,], k)
  }
  return(w)
}

# irisデータを用いたk近傍法のkごとの誤り率を、10-fold CVにより評価。
df <- iris
df <- df[sample(1:150, 150, replace = FALSE), ] # irisデータの並びを乱雑化させる。
n <- nrow(df)
U <- NULL # k近傍法のk値
V <- NULL # 各k値に対応するCV値
for (k in 1:10) { # k近傍法のパラメタkは、1:10で動かす。
  top_seq <- 1 + seq(0, 135, 15)
  S <- 0 # 10-fold CVにおける、各k値でのk近傍法による推定結果の総誤り数
  for (top in top_seq) {
    index <- top:(top + 14)
    knn_ans <- knn(df[-index, 1:4], df[-index, 5], df[index, 1:4], k)
    ans <- df[index, 5]
    S <- S + sum(knn_ans != ans) # k近傍法による分類推定がハズレた場合、Sをインクリメント。
  }
  CV_res <- S / n # 総誤り数をサンプル数で標準化してCVの結果とする。
  U <- c(U, k)
  V <- c(V, CV_res)
}
plot(0, 0,
     type = "n",
     xlab = "k",
     ylab = "誤り率",
     xlim = c(1, 10),
     ylim = c(0, 0.1),
     main = "CVによるirisデータのk近傍法誤り率の評価")
lines(U, V, col = "red")