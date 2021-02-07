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

hc_dendroidgram <- function(cluster, dd = "complete") {
  index <- unlist(cluster[[1]])
  y <- unlist(index)
  n <- length(y)
  height <- array(dim = n)
  z <- matrix(0, ncol = 5, nrow = n)
  for (i in 1:n) {
    index[[i]] <- list(y[i])
    height[i] <- 0
  }
  for (k in n:2) {
    dist_min <- Inf
    for (i in 1:(k - 1)) {
      i_0 <- unlist(index[[i]])
      j_0 <- unlist(index[[i + 1]])
      d <- switch(dd, 
                  "complete" = dist_complete(X[i_0, ], X[j_0, ]),
                  "single" = dist_single(X[i_0, ], X[j_0, ]),
                  "centroid" = dist_centroid(X[i_0, ], X[j_0, ]),
                  "average" = dist_average(X[i_0, ], X[j_0, ]))
      if (d < dist_min) {
        dist_min <- d
        i_1 <- i
        j_1 <- i + 1
      }
    }
    i <- 0
    for (h in 1:i_1) {
      i <- i + length(index[[h]])
    }
    j <- i + length(index[[j_1]])
    z[k, 1] <- i - length(index[[i_1]]) / 2 + 0.5
    z[k, 2] <- j - length(index[[j_1]]) / 2 + 0.5
    z[k, 3] <- height[i_1]
    z[k, 4] <- height[j_1]
    z[k, 5] <- dist_min
    index[[i_1]] <- append(index[[i_1]], index[[j_1]])
    if (j_1 < k) {
      for (h in (j_1 + 1):k) {
        index[[h - 1]] <- index[[h]]
        height[h - 1] <- height[h]
      }
    }
    index[[k]] <- NULL
    height[i_1] <- dist_min
  }
  plot(1:n, 1:n, 
       ylim = c(0, 100),
       xlab = "", ylab = "",
       type = "n", xaxt = "n", yaxt = "n",
       bty = "n", main = dd)
  r <- z[2, 5] / 100
  for (k in n:2) {
    z[k, 3:5] <- z[k, 3:5] / r
    segments(z[k, 1], z[k, 3], z[k, 1], z[k, 5])
    segments(z[k, 1], z[k, 5], z[k, 2], z[k, 5])
    segments(z[k, 2], z[k, 5], z[k, 2], z[k, 4])
  }
  for (i in 1:n) {
    text(i, 0, y[i])
  }
}

n <- 30
p <- 3
X <- matrix(rnorm(n * p), ncol = p, nrow = n)
par(mfrow = c(2, 2))
for (d in c("complete", "single", "centroid", "average")) {
  cluster <- hc(X, dd = d)
  hc_dendroidgram(cluster, dd = d)
}