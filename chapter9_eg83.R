# Load library

# Clear environment
rm(list = ls())

# Setting Japanese font (for MacOSX)
# par(family= "HiraKakuProN-W3")

# Chapter9 e.g.83
# 階層的クラスタリング
dist_complete <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  r <- nrow(x)
  s <- nrow(y)
  dist_max <- 0
  for (i in 1:r) {
    for (j in 1:s) {
      d <- norm(x[i, ] - y[j, ], "2")
      if (d > dist_max) {
        dist_max <- d
      }
    }
  }
  return(dist_max)
}

dist_single <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  r <- nrow(x)
  s <- nrow(y)
  dist_min <- Inf
  for (i in 1:r) {
    for (j in 1:s) {
      d <- norm(x[i, ] - y[j, ], "2")
      if (d < dist_min) {
        dist_min <- d
      }
    }
  }
  return(dist_min)
}

dist_centroid <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  r <- nrow(x)
  s <- nrow(y)
  x_bar <- 0
  for (i in 1:r) {
    x_bar <- x_bar + x[i, ]
    x_bar <- x_bar / r
  }
  y_bar <- 0
  for (i in 1:s) {
    y_bar <- y_bar + y[i, ]
    y_bar <- y_bar / s
  }
  return(norm(x_bar - y_bar, "2") ^ 2)
}

dist_average <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  r <- nrow(x)
  s <- nrow(y)
  S <- 0
  for (i in 1:r) {
    for (j in 1:s) {
      d <- norm(x[i, ] - y[j, ], "2")
      S <- S + d
    }
  }
  return(S / r / s)
}

hc <- function(X, dd = "complete") {
  n <- nrow(X)
  index <- list()
  for (i in 1:n) {
    index[[i]] <- list(i)
  }
  cluster <- list()
  for (k in n:2) {
    dist_min <- Inf
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        i_0 <- unlist(index[[i]])
        j_0 <- unlist(index[[j]])
        d <- switch(dd,
                    "complete" = dist_complete(X[i_0, ], X[j_0, ]),
                    "single" = dist_single(X[i_0, ], X[j_0, ]),
                    "centroid" = dist_centroid(X[i_0, ], X[j_0, ]),
                    "average" = dist_average(X[i_0, ], X[j_0, ]))
        if (d < dist_min) {
          dist_min <- d
          i_1 <- i
          j_1 <- j
        }
      }
    }
    index[[i_1]] <- append(index[[i_1]], index[[j_1]])
    if (j_1 < k) {
      for (h in (j_1 + 1):k) {
        index[[h - 1]] <- index[[h]]
      }
    }
    index[[k]] <- NULL
    cluster[[k - 1]] <- index
  }
  return(cluster)
}

n <- 200
p <- 2
X <- matrix(rnorm(n * p), ncol = p, nrow = n)
cluster <- hc(X)
par(mfrow = c(2, 2))
for (K in c(3, 5, 7, 9)) {
  grp <- cluster[[K]]
  plot(-3:3, -3:3, 
       xlab = "第1成分", ylab = "第2成分",
       type = "n",
       main = paste("K = ", K))
  for (k in 1:K) {
    z <- unlist(grp[[k]])
    x <- X[z, 1]
    y <- X[z, 2]
    points(x, y, col = k + 1)
  }
}
par(mfrow = c(1, 1))