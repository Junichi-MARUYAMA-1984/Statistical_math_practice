# Load library
library(MASS)
library(igraph)

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter7 e.g.69
# ブースティングの実装。

# 最頻値計算用関数mode(y)
mode <- function(y) {
  names(sort(table(y), decreasing = TRUE))[1]
}

# 分岐処理の際の損失関数(1) sq_loss(y)
# 分岐先のデータ集合における応答変数値の分散を返す。
# 分散が小さい（=似たような応答変数値のデータが割り振られている）割り振り方を
# より適切な分岐の仕方であると判定する。
sq_loss <- function(y) {
  y_bar <- mean(y)
  return(sum((y - y_bar) ^ 2))
}

# 分岐処理の際の損失関数(2) mis_match(y)
# 分岐先のデータ集合において、
# その集合内の応答変数最頻値と異なる応答変数値を取るデータの数を返す。
mis_match <- function(y) {
  y_hat <- mode(y)
  return(sum(y != y_hat))
}

# 分岐処理の際の損失関数(3) Gini(y)
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

# 分岐処理の際の損失関数(4) entropy(y)
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

# 分岐処理を行う関数branch(x, y, f, S, m)ランダムフォレストバージョン
# x: 全説明変数データ。説明変数行列の形で受け取る。
# y: 全応答変数データ。
# f: 分岐を評価する損失関数
# S: 分岐を評価するデータの添字集合
# m: 分岐評価に用いる説明変数要素の数。デフォルトは全要素。
branch <- function(x, y, f, S, m = ncol(x)) {
  n <- length(S) # 分岐を評価するデータ集合に含まれるデータ数
  p <- ncol(x) # 説明変数に含まれる要素数
  if (n == 0) { # 評価するデータ集合が空集合ならばNULLを返す。
    return(NULL)
  }
  best_score <- Inf # 分岐評価スコアの最小値
  if (m < p) {
    T <- sample(1:p, m, replace = FALSE) # mがp以外の場合、説明変数の要素を重複を許さずにランダムにm個選択。
  } else {
    T <- 1:p
  }
  for (j in T) { # 分岐評価に用いる要素として、説明変数要素を添字集合Tに従ってなめる。
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

# ブースティング決定木構成を行う関数b_dt(x, y, d, f)
# x: 全説明変数データ。説明変数行列の形で受け取る。
# y: 全応答変数データ。
# d: ブースティングで発生させる木の分岐数。（頂点の個数は2d+1個となる。）
# f: 分岐を評価する損失関数。デフォルト引数としてsq_lossを取る。
b_dt <- function(x, y, d, f = "sq_loss") {
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
  vertex <- list() # i番目の頂点に関する属性情報を保持するlist型変数。
  # vertex[[i]]の各要素について：
  # parent: 親頂点の番号。
  # set: その頂点に割り振られたデータの添字集合。
  # th: （子頂点を持っている場合、）分岐処理を行った際の基準値として用いたデータ値。
  # j: （子頂点を持っている場合、）分岐処理を行った際の基準として用いた「説明変数の要素」の番号。端点では0とする。
  vertex[[1]] <- list(parent = 0, set = 1:n, score = g(y), j = 0)
  
  # 指定した分岐の数だけ分岐処理を行う。
  while (length(vertex) <= 2 * d - 1) {
    r <- length(vertex) # 木に含まれる頂点数。
    gain_max <- -Inf # 損失関数の最大改善量
    # 全頂点をなめて、端点において分岐処理を行いその損失関数改善量を評価する。
    # 分岐処理は最善の損失関数改善量を叩いた端点でのみ行う。
    for (h in 1:r) { 
      if (vertex[[h]]$j == 0) { # 頂点hが端点ならば分岐処理を行う。
        res <- branch(x, y, g, vertex[[h]]$set) # 分岐処理
        gain <- vertex[[h]]$score - res$score # 損失関数値の改善量
        if (gain > gain_max) { 
          gain_max <- gain
          h_max <- h # 損失関数値が最も改善した際に分岐処理を実行した親頂点番号。
          res_max <- res # 損失関数値が最も改善した際の分岐処理で得られた子頂点の情報。
        }
      }
    }
    # 最善の損失関数改善量を叩いた分岐処理の情報をvertexに記録。
    vertex[[h_max]]$th <- x[res_max$i, res_max$j]
    vertex[[h_max]]$j <- res_max$j
    vertex[[r + 1]] <- list(parent = h_max, set = res_max$left,
                            score = res_max$left_score, j = 0)
    vertex[[r + 2]] <- list(parent = h_max, set = res_max$right,
                            score = res_max$right_score, j = 0)
  }
  
  # 出来上がった木データvertexの整形。
  # 左右の子頂点の番号情報をleft, rightとして付与する。端点の場合は値を0とする。
  # 頂点が端点であれば、割り振られたデータの応答変数値の最頻値情報または平均値情報をcenterとして付与する。
  # このcenterが、決定木を用いた回帰における応答変数推定値となる。
  
  r <- 2 * d + 1 # 最終的に出来上がった木（vertex）に含まれる頂点数
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
    if (vertex[[h]]$right == 0 & vertex[[h]]$left == 0) {
      vertex[[h]]$j <- 0
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

# Bostonデータセットを用いてブースティングを実行。
# データ生成
df <- Boston
x <- as.matrix(df[, 1:13])
y <- as.matrix(df[, 14])
train <- 1:200 # 学習用データ
test <- 201:300 # テストデータ
B <- 100 # ブースティングで作成する決定木の個数
lambda <- 0.1 # ブースティング式中のlambda（shrinking factor）
d <- 2 # 一個当たりの決定木中に含まれる内点の個数
trees <- list() # 作成された決定木を保存するリスト型変数
r <- y[train] # 応答変数値の正解値
for (b in 1:B) {
  trees[[b]] <- b_dt(x[train, ], r, d) # 決定木作成
  for (i in train) {
    r[i] <- r[i] - lambda * value(x[i, ], trees[[b]]) # rの更新処理
  }
}
z <- array(0, dim = c(B, 600)) # 決定木による応答変数推定値記録用配列
for (i in test) {
  z[1, i] <- lambda * value(x[i, ], trees[[1]]) # b = 1の決定木による推定値計算
}
for (b in 2:B) {
  for (i in test) {
    z[b, i] <- z[b - 1, i] + lambda * value(x[i, ], trees[[b]]) # b = 1以降のbに関する決定木を用いて推定値を逐次計算
  }
}
out <- NULL # 各b値における決定木を用いた応答変数推定値の二乗誤差を記録する配列
for (b in 1:B) {
  out <- c(out, sum((y[test] - z[b, test]) ^ 2) / length(test))
}
plot(21:B, 
     xlab = "生成した木の個数", ylab = "テストデータでの二乗誤差",
     type = "n",
     xlim = c(20, 100), ylim = c(0, 35),
     main = "ブースティング")
lines(21:B, out[21:100], col = "blue")
legend("topright",
       legend = c("d = 2"),
       col = c("blue"),
       lty = 1)

# テキスト中ではd = 1, d = 3の場合も計算しているが、
# このままでは計算時間がめちゃくちゃかかるので、
# もしやるのならばコードの効率化が必要。