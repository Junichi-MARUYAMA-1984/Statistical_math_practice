# Load library
library(MASS)
library(igraph)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter7 e.g.66
# 分類の決定木。

# 最頻値計算用関数mode(y)
mode <- function(y) {
  names(sort(table(y), decreasing = TRUE))[1]
}

# 分岐処理の際の損失関数(1) mis_match(y)
# 分岐先のデータ集合において、
# その集合内の応答変数最頻値と異なる応答変数値を取るデータの数を返す。
mis_match <- function(y) {
  y_hat <- mode(y)
  return(sum(y != y_hat))
}

# 分岐処理の際の損失関数(2) Gini(y)
# 分岐先のデータ集合におけるGini指標を返す。
# Gini指標が小さければ小さいほど最適なデータ割り振りと判定する。
gini <- function(y) {
  n <- length(y)
  if (n == 0) {
    return(0)
  }
  z <- as.vector(table(y))
  m <- length(z)
  T <- 0
  for (j in 1:m) {
    T <- T + z[j] * (n - z[j])
  }
  return(T / n)
}

# 分岐処理の際の損失関数(3) entropy(y)
# 分岐先のデータ集合におけるentropyを返す。
# entropyが小さければ小さいほど最適なデータ割り振りと判定する。
entropy <- function(y) {
  n <- length(y)
  if (n == 0) {
    return(0)
  }
  z <- as.vector(table(y))
  m <- length(z)
  T <- 0
  for (j in 1:m) {
    if (z[j] != 0) {
      T <- T + z[j] * log(n / z[j])
    }
  }
  return(T)
}

# 分岐処理を行う関数branch(x, y, f, S, m)
# x: 全説明変数データ。説明変数行列の形で受け取る。
# y: 全応答変数データ。
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

# 決定木構成を行う関数dt(x, y, f, alpha, n_min, m)
# x: 全説明変数データ。説明変数行列の形で受け取る。
# y: 全応答変数データ。
# f: 分岐を評価する損失関数。デフォルト引数としてsq_lossを取る。
# alpha: 式(7.3)におけるパラメタalpha。
# n_min: 各頂点において分岐処理を行うか否かを決定する閾値データ数。
# m: 説明変数に含まれる要素数。
dt <- function(x, y, 
               f = "sq_loss", alpha = 0, n_min = 1, m = ncol(x)) {
  # 損失関数の決定
  if (f == "sq_loss") {
    g <- sq_loss
  } else if (f == "mis_match") {
    g <- mis_match
  } else if (f == "gini") {
    g <- gini
  } else {
    g <- entropy
  }
  
  n <- length(y) # 総データ数
  stack <- list() # 決定木構成に用いるスタック。
  # stackの各要素stack[[i]]は、
  # i番目の頂点に関する属性情報を保持するlist型変数。
  # parent: 親頂点の番号。
  # set: 子となるデータの添字集合。
  # score: 子となるデータ集合の損失関数値。
  stack[[1]] <- list(parent = 0, set = 1:n, score = g(y)) # i = 1の頂点は根。
  vertex <- list() # i番目の頂点に関する属性情報を保持するlist型変数。
  # vertex[[i]]の各要素について：
  # parent: 親頂点の番号。
  # set: その頂点に割り振られたデータの添字集合。
  # th: （子頂点を持っている場合、）分岐処理を行った際の基準値として用いたデータ値。
  # j: （子頂点を持っている場合、）分岐処理を行った際の基準として用いた「説明変数の要素」の番号。端点では0とする。
  k <- 0 # 頂点番号管理用変数
  
  # 処理待ちスタックが0になるまで分岐処理を行う。
  while (length(stack) > 0) {
    r <- length(stack) # 決定木構成処理待ち頂点の数
    node <- stack[[r]] # スタックの一番上にある頂点を処理対象とする。
    stack <- stack[-r] # POP処理とするので、スタックの一番上にある頂点を削除。
    k <- k + 1
    res <- branch(x, y, g, node$set, m) # 処理対象頂点において分岐処理を実行。
    # 以下の条件に当てはまる場合は、分岐処理を止めて、当該頂点を端点として管理する。
    # i) 分岐処理を行う前と行った後のscore差がalphaより小さい場合。
    # ii) 当該頂点におけるデータ数がn_minより小さい場合。
    # iii) 子頂点に割り振られたデータ集合が一つでも空集合となった場合。
    if (node$score - res$score < alpha || 
        length(node$set) < n_min ||
        length(res$left) == 0 ||
        length(res$right) == 0) {
      vertex[[k]] <- list(parent = node$parent, j = 0, set = node$set)
    } else {
      # 分岐処理を行う場合は、当該頂点を内点として管理する。
      # かつ、生成された子頂点の情報を処理待ちスタックに追加する。
      vertex[[k]] <- list(parent = node$parent, set = node$set, 
                          th = x[res$i, res$j], j = res$j)
      stack[[r]] <- list(parent = k, set = res$right, score = res$right_score)
      stack[[r + 1]] <- list(parent = k, set = res$left, score = res$left_score)
    }
  }
  
  # 出来上がった木データvertexの整形。
  # 左右の子頂点の番号情報をleft, rightとして付与する。端点の場合は値を0とする。
  # 頂点が端点であれば、割り振られたデータの応答変数値の最頻値情報または平均値情報をcenterとして付与する。
  # このcenterが、決定木を用いた回帰における応答変数推定値となる。
  
  r <- length(vertex) # 木に含まれる頂点数
  for (h in 1:r) {
    vertex[[h]]$left <- 0; vertex[[h]]$right <- 0 # 各頂点の左右子頂点の番号を0で初期化。
  }
  for (h in r:2) { # 端点から根に向かって遡って処理を行う。
    pa <- vertex[[h]]$parent # 当該頂点の親頂点の頂点番号。
    if (vertex[[pa]]$right == 0) { # 子頂点番号情報は左の方が小さくなるように付与する。
      vertex[[pa]]$right <- h
    } else {
      vertex[[pa]]$left <- h
    }
  }
  # 損失関数がsq_lossの場合は、当該頂点における応答変数値の平均値情報を付与する。
  # それ以外の損失関数の場合は、当該頂点における応答変数値の最頻値情報を付与する。
  if (f == "sq_loss") {
    g <- mean
  } else {
    g <- mode
  }
  for (h in 1:r) {
    if (vertex[[h]]$j == 0) { # 以下の処理は端点の時のみ。
      vertex[[h]]$center <- g(y[vertex[[h]]$set]) # 最頻値情報or平均値情報をcenterとして付与。
    }
  }
  return(vertex)
}

# 決定木から応答変数推定値を取得する関数value(u, vertex)
# u: 応答変数推定値を取得する対象の説明変数値（∈R^p）。
# vertex: dt()で作成した決定木。
value <- function(u, vertex) {
  r <- 1 # 根からスタートして、その説明変数値が割り振られる端点に降りていく。
  while (vertex[[r]]$j != 0) {
    if (u[vertex[[r]]$j] < vertex[[r]]$th) {
      r <- vertex[[r]]$left
    } else {
      r <- vertex[[r]]$right
    }
  }
  return(vertex[[r]]$center) # 辿り着いた端点の応答変数代表値を返す。
}

# irisデータセットを用いて、決定木による分類を行う。
# 1) 損失関数としてmis_match()を使用した場合
df <- iris
x <- as.matrix(df[, 1:4])
y <- as.matrix(df[, 5])
vertex <- dt(x, y, "mis_match", n_min = 4)
m <- length(vertex)
u <- NULL # 各端点のデータ集合の応答変数値をすべてその最頻値に置き換えて並べた配列。
v <- NULL # 各端点のデータ集合の実際の応答変数値を並べた配列。
for (h in 1:m) { # 分類正誤確認用テーブルの作成処理
  if (vertex[[h]]$j == 0) {
    w <- y[vertex[[h]]$set]
    u <- c(u, rep(mode(w), length(w)))
    v <- c(v, w)
  }
}
col <- array(dim = m)
edge_list <- matrix(nrow = m, ncol = 2)
for (h in 1:m) {
  col[h] <- vertex[[h]]$j
  edge_list[h, ] <- c(vertex[[h]]$parent, h)
}
edge_list <- edge_list[-1, ]
g <- graph_from_edgelist(edge_list)
V(g)$color <- col
# V(g)$name <- ""
plot(g, pin = c(4, 4),
     layout = layout.reingold.tilford(g, root = 1))
title("誤り率")
# 各分岐処理のログを表示
res_table_mismatch <- table(u, v)
NODE <- NULL # 分岐処理を行った頂点（内点）の番号
VAR <- NULL # 分岐処理の基準として用いた説明変数の要素の番号
TH <- NULL # 分岐処理の基準値として用いたデータ値
ENDP <- NULL # 端点の番号
EST <- NULL # 端点における応答変数推定値
for (h in 1:m) {
  if (vertex[[h]]$j != 0) {
    j <- vertex[[h]]$j
    th <- vertex[[h]]$th
    NODE <- c(NODE, h)
    VAR <- c(VAR, j)
    TH <- c(TH, th)
  } else {
    center <- vertex[[h]]$center
    ENDP <- c(ENDP, h)
    EST <- c(EST, center)
  }
}
th_mismatch <- cbind(NODE, VAR, TH)
est_mismatch <- cbind(ENDP, EST)
print(res_table_mismatch)
print(th_mismatch)
print(est_mismatch)

# 2) 損失関数としてgini()を使用した場合
vertex <- dt(x, y, "gini", n_min = 4)
m <- length(vertex)
u <- NULL # 各端点のデータ集合の応答変数値をすべてその最頻値に置き換えて並べた配列。
v <- NULL # 各端点のデータ集合の実際の応答変数値を並べた配列。
for (h in 1:m) { # 分類正誤確認用テーブルの作成処理
  if (vertex[[h]]$j == 0) {
    w <- y[vertex[[h]]$set]
    u <- c(u, rep(mode(w), length(w)))
    v <- c(v, w)
  }
}
col <- array(dim = m)
edge_list <- matrix(nrow = m, ncol = 2)
for (h in 1:m) {
  col[h] <- vertex[[h]]$j
  edge_list[h, ] <- c(vertex[[h]]$parent, h)
}
edge_list <- edge_list[-1, ]
g <- graph_from_edgelist(edge_list)
V(g)$color <- col
# V(g)$name <- ""
plot(g, pin = c(4, 4),
     layout = layout.reingold.tilford(g, root = 1))
title("Gini")
# 各分岐処理のログを表示
res_table_gini <- table(u, v)
NODE <- NULL # 分岐処理を行った頂点（内点）の番号
VAR <- NULL # 分岐処理の基準として用いた説明変数の要素の番号
TH <- NULL # 分岐処理の基準値として用いたデータ値
ENDP <- NULL # 端点の番号
EST <- NULL # 端点における応答変数推定値
for (h in 1:m) {
  if (vertex[[h]]$j != 0) {
    j <- vertex[[h]]$j
    th <- vertex[[h]]$th
    NODE <- c(NODE, h)
    VAR <- c(VAR, j)
    TH <- c(TH, th)
  } else {
    center <- vertex[[h]]$center
    ENDP <- c(ENDP, h)
    EST <- c(EST, center)
  }
}
th_gini <- cbind(NODE, VAR, TH)
est_gini <- cbind(ENDP, EST)
print(res_table_gini)
print(th_gini)
print(est_gini)

# 3) 損失関数としてentropy()を使用した場合
vertex <- dt(x, y, "entropy", n_min = 4)
m <- length(vertex)
u <- NULL # 各端点のデータ集合の応答変数値をすべてその最頻値に置き換えて並べた配列。
v <- NULL # 各端点のデータ集合の実際の応答変数値を並べた配列。
for (h in 1:m) { # 分類正誤確認用テーブルの作成処理
  if (vertex[[h]]$j == 0) {
    w <- y[vertex[[h]]$set]
    u <- c(u, rep(mode(w), length(w)))
    v <- c(v, w)
  }
}
col <- array(dim = m)
edge_list <- matrix(nrow = m, ncol = 2)
for (h in 1:m) {
  col[h] <- vertex[[h]]$j
  edge_list[h, ] <- c(vertex[[h]]$parent, h)
}
edge_list <- edge_list[-1, ]
g <- graph_from_edgelist(edge_list)
V(g)$color <- col
# V(g)$name <- ""
plot(g, pin = c(4, 4),
     layout = layout.reingold.tilford(g, root = 1))
title("Entropy")
# 各分岐処理のログを表示
res_table_entropy <- table(u, v)
NODE <- NULL # 分岐処理を行った頂点（内点）の番号
VAR <- NULL # 分岐処理の基準として用いた説明変数の要素の番号
TH <- NULL # 分岐処理の基準値として用いたデータ値
ENDP <- NULL # 端点の番号
EST <- NULL # 端点における応答変数推定値
for (h in 1:m) {
  if (vertex[[h]]$j != 0) {
    j <- vertex[[h]]$j
    th <- vertex[[h]]$th
    NODE <- c(NODE, h)
    VAR <- c(VAR, j)
    TH <- c(TH, th)
  } else {
    center <- vertex[[h]]$center
    ENDP <- c(ENDP, h)
    EST <- c(EST, center)
  }
}
th_entropy <- cbind(NODE, VAR, TH)
est_entropy <- cbind(ENDP, EST)
print(res_table_entropy)
print(th_entropy)
print(est_entropy)