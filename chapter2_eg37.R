# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter2 e.g.37
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

# irisデータにおけるアヤメ種判別をk近傍法で実行
df <- iris # irisデータ読み込み
n <- nrow(iris) # データ数取得（150個）
train <- sample(1:n, n / 2, replace = FALSE) # 1:150からランダムに75個の整数を選択する。
test <- setdiff(1:n, train) # 1:150から、trainとして選択されなかった整数75個を取得。
x <- as.matrix(df[train, 1:4]) # 訓練データの説明変数データ
y <- as.vector(df[train, 5]) # 訓練データの応答変数データ（アヤメ種名）
z <- as.matrix(df[test, 1:4]) # テストデータの説明変数データ
ans <- as.vector(df[test, 5]) # テストデータの応答変数値の正解データ
w <- knn(x, y, z, k = 3) # k近傍法を実行
table(w, ans) # 判別結果と正解データの比較