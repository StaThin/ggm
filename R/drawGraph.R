drawGraph <- function (amat, coor = NULL, adjust = FALSE, alpha = 1.5, beta = 3, lwd = 1, ecol = "blue", bda = 0.1, layout = layout.auto) 
{
  if (is.null(dimnames(amat))) {
    rownames(a) <- 1:ncol(amat)
    colnames(a) <- 1:ncol(amat)
  }
  if (all(amat == t(amat)) & all(amat[amat != 0] == 1)) {
    amat <- amat * 10
  }
  lay <- function(a, directed = TRUE, start = layout) {
    if (class(a)[1] == "igraph" || class(a)[1] == "graphNEL" || 
        class(a)[1] == "character") {
      a <- grMAT(a)
    }
    if (is(a, "matrix")) {
      if (nrow(a) == ncol(a)) {
        if (length(rownames(a)) != ncol(a)) {
          rownames(a) <- 1:ncol(a)
          colnames(a) <- 1:ncol(a)
        }
        if (!directed) {
          if (all(a == t(a)) & all(a[a != 0] == 1)) {
            a <- a * 10
          }
        }
        l1 <- c()
        l2 <- c()
        for (i in 1:nrow(a)) {
          for (j in i:nrow(a)) {
            if (a[i, j] == 1) {
              l1 <- c(l1, i, j)
              l2 <- c(l2, 2)
            }
            if (a[j, i]%%10 == 1) {
              l1 <- c(l1, j, i)
              l2 <- c(l2, 2)
            }
            if (a[i, j] == 10) {
              l1 <- c(l1, i, j)
              l2 <- c(l2, 0)
            }
            if (a[i, j] == 11) {
              l1 <- c(l1, i, j, i, j)
              l2 <- c(l2, 2, 0)
            }
            if (a[i, j] == 100) {
              l1 <- c(l1, i, j)
              l2 <- c(l2, 3)
            }
            if (a[i, j] == 101) {
              l1 <- c(l1, i, j, i, j)
              l2 <- c(l2, 2, 3)
            }
            if (a[i, j] == 110) {
              l1 <- c(l1, i, j, i, j)
              l2 <- c(l2, 0, 3)
            }
            if (a[i, j] == 111) {
              l1 <- c(l1, i, j, i, j, i, j)
              l2 <- c(l2, 2, 0, 3)
            }
          }
        }
      }
      else {
        stop("'object' is not in a valid adjacency matrix form")
      }
      if (length(l1) > 0) {
        agr <- graph(l1, n = nrow(a), directed = TRUE)
      }
    }
    else {
      stop("'object' is not in a valid format")
    }
    x <- start(agr)
    x[, 1] <- 10 + 80 * (x[, 1] - min(x[, 1]))/(max(x[, 1]) - 
                                                  min(x[, 1]))
    x[, 2] <- 10 + 80 * (x[, 2] - min(x[, 2]))/(max(x[, 2]) - 
                                                  min(x[, 2]))
    x
  }
  plot.dots <- function(xy, v, dottype, n, beta) {
    for (i in 1:n) {
      if (dottype[i] == 1) {
        points(xy[i, 1], xy[i, 2], pch = 1, cex = 1.2, 
               lwd = lwd)
      }
      else if (dottype[i] == 2) {
        points(xy[i, 1], xy[i, 2], pch = 16, cex = 1.2)
      }
    }
    text(xy[, 1] - beta, xy[, 2] + 2 * beta, v, cex = 1.2)
  }
  angle <- function(v, alpha = pi/2) {
    theta <- Arg(complex(real = v[1], imaginary = v[2]))
    z <- complex(argument = theta + alpha)
    c(Re(z), Im(z))
  }
  double.edges <- function(x1, x2, y1, y2, lwd, ecol) {
    d <- 50
    n <- 30
    dd <- 2
    k <- length(x1)
    if (is.na(x1)) {
      return()
    }
    for (i in 1:k) {
      x <- c(x1[i], x2[i])
      y <- c(y1[i], y2[i])
      m <- (x + y)/2
      cen <- m + d * angle(y - x)
      xm <- x - cen
      ym <- y - cen
      thetax <- Arg(complex(real = xm[1], imaginary = xm[2]))
      thetay <- Arg(complex(real = ym[1], imaginary = ym[2]))
      theta <- seq(thetax, thetay, len = n)
      l <- crossprod(y - m)
      delta <- sqrt(d^2 + l)
      lx <- cen[1] + delta * cos(theta)
      ly <- cen[2] + delta * sin(theta)
      lines(lx, ly, lty = 2, col = ecol, lwd = lwd)
      vy <- angle(y - cen)
      vx <- angle(x - cen)
      vx1 <- x + dd * angle(vx, alpha = pi/12)
      vx2 <- x + dd * angle(vx, alpha = -pi/12)
      vy1 <- y + dd * angle(vy, alpha = 11 * pi/12)
      vy2 <- y + dd * angle(vy, alpha = -11 * pi/12)
      segments(x[1], x[2], vx1[1], vx1[2], col = ecol, 
               lwd = lwd)
      segments(x[1], x[2], vx2[1], vx2[2], col = ecol, 
               lwd = lwd)
      segments(y[1], y[2], vy1[1], vy1[2], col = ecol, 
               lwd = lwd)
      segments(y[1], y[2], vy2[1], vy2[2], col = ecol, 
               lwd = lwd)
      ex <- x + 0.05 * (y - x)
      ey <- x + 0.95 * (y - x)
      arrows(ex[1], ex[2], ey[1], ey[2], lty = 1, code = 1, 
             angle = 20, length = 0.1, lwd = lwd, col = ecol)
    }
  }
  draw.edges <- function(coor, u, alpha, type, lwd, ecol, bda) {
    for (k in 1:nrow(u)) {
      a <- coor[u[k, 1], ]
      b <- coor[u[k, 2], ]
      ba <- b - a
      ba <- ba/sqrt(sum(ba * ba))
      x <- a + ba * alpha
      y <- b - ba * alpha
      switch(type + 1, segments(x[1], x[2], y[1], y[2], 
                                lty = 1, lwd = lwd, col = ecol), arrows(x[1], 
                                                                        x[2], y[1], y[2], code = 2, angle = 20, length = 0.1, 
                                                                        lty = 1, lwd = lwd, col = ecol), arrows(x[1], 
                                                                                                                x[2], y[1], y[2], code = 3, angle = 20, length = bda, 
                                                                                                                lty = 5, lwd = lwd, col = ecol), double.edges(x[1], 
                                                                                                                                                              x[2], y[1], y[2], lwd = lwd, ecol))
    }
  }
  v <- parse(text = rownames(amat))
  n <- length(v)
  dottype <- rep(1, n)
  old <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old))
  plot(c(0, 100), c(0, 100), type = "n", axes = FALSE, xlab = "", 
       ylab = "")
  center <- matrix(c(50, 50), ncol = 2)
  if (is.null(coor)) {
    coor <- lay(amat)
  }
  g0 <- amat * ((amat == 10) & (t(amat) == 10))
  g0[lower.tri(g0)] <- 0
  g1 <- amat * ((amat == 1) & !((amat > 0) & (t(amat) > 0)))
  g2 <- amat * ((amat == 100) & (t(amat) == 100))
  g2[lower.tri(g2)] <- 0
  g3 <- (amat == 101) + 0
  i <- expand.grid(1:n, 1:n)
  u0 <- i[g0 > 0, ]
  u1 <- i[g1 > 0, ]
  u2 <- i[g2 > 0, ]
  u3 <- i[g3 > 0, ]
  if (nrow(coor) != length(v)) {
    stop("Wrong coordinates of the vertices.")
  }
  plot.dots(coor, v, dottype, n, beta)
  draw.edges(coor, u0, alpha, type = 0, lwd = lwd, ecol, bda)
  draw.edges(coor, u1, alpha, type = 1, lwd = lwd, ecol, bda)
  draw.edges(coor, u2, alpha, type = 2, lwd = lwd, ecol, bda)
  draw.edges(coor, u3, alpha, type = 3, lwd = lwd, ecol, bda)
  if (adjust) {
    repeat {
      xnew <- unlist(locator(1))
      if (length(xnew) == 0) {
        break
      }
      d2 <- (xnew[1] - coor[, 1])^2 + (xnew[2] - coor[, 
                                                      2])^2
      i <- (1:n)[d2 == min(d2)]
      coor[i, 1] <- xnew[1]
      coor[i, 2] <- xnew[2]
      plot(c(0, 100), c(0, 100), type = "n", axes = FALSE, 
           xlab = "", ylab = "")
      plot.dots(coor, v, dottype, n, beta)
      draw.edges(coor, u0, alpha, type = 0, lwd = lwd, 
                 ecol, bda)
      draw.edges(coor, u1, alpha, type = 1, lwd = lwd, 
                 ecol, bda)
      draw.edges(coor, u2, alpha, type = 2, lwd = lwd, 
                 ecol, bda)
      draw.edges(coor, u3, alpha, type = 3, lwd = lwd, 
                 ecol, bda)
    }
  }
  colnames(coor) <- c("x", "y")
  return(invisible(coor))
}