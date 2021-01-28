# Load library
library(MASS)
library(igraph)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter7 e.g.64
# 回帰の決定木。

# 分岐処理の際の損失関数sq_loss(y)
# 分岐先のデータ集合における応答変数値の分散を返す。
sq_loss <- function(y) {
  y_bar <- mean(y)
  return(sum((y - y_bar) ^ 2))
}

# 分岐処理を行う関数branch(x, y, f, S, m)
# x: 全説明変数データ。説明変数行列の形で受け取る。
# y: 全応答変数データ
# f: 分岐を評価する損失関数
# S: 分岐を評価するデータの添字集合
# m: 説明変数に含まれる要素数（実際は利用されていない！）
branch <- function(x, y, f, S, m = ncol(x)) {
  n <- length(S) # 分岐を評価するデータ集合に含まれるデータ数
  p <- ncol(x) # 説明変数に含まれる要素数
  if (n == 0) { # 評価するデータ集合が空集合ならばNULLを返す。
    return(NULL)
  }
  best_score <- Inf # 分岐評価スコアの最小値
  for (j in 1:p) { # 分岐評価に用いる要素として、説明変数の全要素をなめる。
    for (i in S) { # 分岐評価に用いるデータとして、評価対象データ集合の全データをなめる。
      left <- NULL; right <- NULL # 分岐先のデータ集合
      for (k in S) {
        if (x[k, j] < x[i, j]) { # i番目のデータの第j要素を基準値として、各データの第j要素を評価する。
          left <- c(left, k) # 基準値より大きかったら左の分岐へ割り振る。
        } else {
          right <- c(right, k) # 基準値より小さかったら右の分岐へ割り振る。
        }
      }
      L <- f(y[left]); R <- f(y[right]) # 割り振られた分岐先のデータ集合において、損失関数の値を計算。
      score <- L + R # 左右のデータ群の損失関数値の和を、当該分岐条件における評価スコアとする。
      if (score < best_score) {
        best_score <- score
        # ベストスコアを叩いた分岐条件のパラメータを戻り値とする。
        info <- list(i = i, j = j, # 分岐評価の基準値として用いた、「i番目のデータの第j要素」のi, j
                     left = left, right = right, # 左と右の各分岐に割り振られたデータの添字。
                     score = best_score, # ベストスコアの値。
                     left_score = L, right_score = R) # ベストスコアの際の左と右の各データ集合における損失関数値。
      }
    }
  }
  return(info)
}

dt <- function(x, y, 
               f = "sq_loss", alpha = 0, n_min = 1, m = ncol(x)) {
  if (f == "sq_loss") {
    g <- sq_loss
  } else if (f == "mis_match") {
    g <- mis_match
  } else if (f == "gini") {
    g <- gini
  } else {
    g <- entropy
  }
  n <- length(y)
  stack <- list()
  stack[[1]] <- list(parent = 0, set = 1:n, score = g(y))
  vertex <- list()
  k <- 0
  while (length(stack) > 0) {
    r <- length(stack)
    node <- stack[[r]]
    stack <- stack[-r]
    k <- k + 1
    res <- branch(x, y, g, node$set, m)
    if (node$score - res$score < alpha ||
        length(node$set) < n_min ||
        length(res$left) == 0 ||
        length(res$right) == 0) {
      vertex[[k]] <- list(parent = node$parent, j = 0, set = node$set)
    } else {
      vertex[[k]] <- list(parent = node$parent, set = node$set, 
                          th = x[res$i, res$j], j = res$j)
      stack[[r]] <- list(parent = k, set = res$right, score = res$right_score)
      stack[[r + 1]] <- list(parent = k, set = res$left, score = res$left_score)
    }
  }
  mode <- function(y) {
    names(sort(table(y), decreasing = TRUE))[1]
  }
  r <- length(vertex)
  for (h in 1:r) {
    vertex[[h]]$left <- 0; vertex[[h]]$right <- 0
  }
  for (h in r:2) {
    pa <- vertex[[h]]$parent
    if (vertex[[pa]]$right == 0) {
      vertex[[pa]]$right <- h
    } else {
      vertex[[pa]]$left <- h
    }
  }
  if (f == "sq_loss") {
    g <- mean
  } else {
    g <- mode
  }
  for (h in 1:r) {
    if (vertex[[h]]$j == 0) {
      vertex[[h]]$center <- g(y[vertex[[h]]$set])
    }
  }
  return(vertex)
}

x <- as.matrix(Boston[, 1:13])
y <- as.vector(Boston[, 14])
vertex <- dt(x, y, n_min = 50)
r <- length(vertex)
col <- array(dim = r)
edge_list <- matrix(nrow = r, ncol = 2)
for (h in 1:r) {
  col[h] <- vertex[[h]]$j
  edge_list[h, ] <- c(vertex[[h]]$parent, h)
}
edge_list <- edge_list[-1, ]
g <- graph_from_edgelist(edge_list)
V(g)$color <- col
plot(g, layout = layout.reingold.tilford(g, root = 1))
VAR <- NULL
TH <- NULL
for (h in 1:r) {
  if (vertex[[h]]$j != 0) {
    j <- vertex[[h]]$j
    th <- vertex[[h]]$th
    VAR <- c(VAR, j)
    TH <- c(TH, th)
  }
}
print(cbind(VAR, TH))