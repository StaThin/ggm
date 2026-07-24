# Modifications to the fitmlogit function in ggm

`fit_mlogit` <- function(..., C = c(), D = c(), mit = 100, ep = 1e-80, acc = 0.0001, daf) {
  # Fits a logistic regression model to multivariate binary responseses
  
  loglin2 <- function(d) {
    # Finds the matrix G for a set o d binary variables in inv lex order.
    G <- 1
    K <- matrix(c(1, 1, 0, 1), 2, 2)
    for (i in 1:d) {
      G <- G %x% K
    }
    G[, -1]
  }
  blodiag <- function(x, blo) {
    # Split a vector x into a block diagonal matrix bith components blo.
    k <- length(blo)
    B <- matrix(0, k, sum(blo))
    u <- cumsum(c(1, blo))
    for (i in 1:k) {
      sub <- u[i]:(u[i + 1] - 1)
      if(blo[i] == 0) {
        B[i, ] <- 0
      }
      else {
        B[i, sub] <- x[sub]
      }
    }
    B
  }
  
  mods <- list(...) # mods should have 2^q - 1 components
  nm <- length(mods)
  be <- c()
  # Starting values
  resp <- c()
  Xbig <- c()
  blo <- c()
  for (k in 1:nm) {
    mf <- model.frame(mods[[k]], data = daf)
    res <- model.response(mf)
    Xsmall <- model.matrix(mods[[k]], data = daf)
    Xbig <- cbind(Xbig, Xsmall)
    blo <- c(blo, ncol(Xsmall))
    nr <- 1
    if (!is.matrix(res)) { # CORRECTED !
      b <- glm(mods[[k]], family = binomial, data = daf) # daf instead of data
      be <- c(be, coef(b))
    } else {
      be2 <- rep(0.1, ncol(Xsmall))
      be <- c(be, be2)
      nc <- ncol(res)
      if (nc > nr) {
        nr <- nc
        Y <- res
      }
    }
  }


  q <- nr # number of responses
  b <- rep(2, q) # all binary responses
  # Transforms the binary observation into a cell number
  y <- 1 + (Y %*% 2^(0:(q - 1)))

  # Finds the matrices C, M and G

  mml <- mat.mlogit(q)
  Co <- mml$C
  Ma <- mml$L
  Co <- as.matrix(Co)
  G <- loglin2(q)
  b0 <- be
  n <- length(y) # le righe di y sono le unità
  t <- max(y) #  Questo è semplicemente 2^q
  k <- length(be) # number of parameters
  rc <- nrow(C)
  cc <- ncol(C)
  rd <- nrow(D)
  cd <- ncol(D)
  if (!is.null(C)) { # se C non ha zero righe trova il null space di C
    U <- null(C)
  }
  seta <- nrow(Co) # e' la dimensione di eta
  mg <- t(G) %*% matrix(1 / t, t, t) # NB troppi t!
  H <- solve(crossprod(G) - mg %*% G) %*% (t(G) - mg)

  # initialize

  P <- matrix(0, t, n)
  cat("Initial probabilities\n")
  for (iu in 1:n) { #  initialize P iu = index of a unit
    #   X = .bdiag(lapply(mods, function(x) model.matrix(x, data = data[iu,])))    ### Change this
    #   X = as.matrix(X)

    X <- blodiag(Xbig[iu, ], blo) # <- PROBLEM if cbind(X1, X2) ~ 0
    eta <- X %*% be
    eta <- as.matrix(eta)
    p <- binve(eta, Co, Ma, G)
    p <- pmax(p, ep)
    p <- p / sum(p)
    P[, iu] <- p
  }

  # Iterate

  it <- 0
  test <- 0
  diss <- 1
  LL0 <- 0
  dis <- 1
  dm <- 1
  while (it < mit && (dis + diss) > acc) {
    LL <- 0
    s <- matrix(0, k, 1)
    S <- matrix(0, k, k)
    dis <- 0
    for (iu in 1:n) {
      # X = .bdiag(lapply(mods, function(x) model.matrix(x, data = data[iu,])))
      # X = as.matrix(X)
 #     browser()
      X <- blodiag(Xbig[iu, ], blo)
      p <- P[, iu]
      if (it > 0) {
        Op <- diag(p) - p %*% t(p)
        R <- Co %*% diagv(1 / (Ma %*% p), Ma) %*% Op %*% G #   This is the inverse Jacobian
        while (rcond(R) < 0.000000000001) {
          R <- R + diag(seta)
        }
        R <- solve(R)
        delta <- X %*% be - Co %*% log(Ma %*% p)
        th <- H %*% log(p) + R %*% delta
        dm <- max(th) - min(th)
        p <- exp(G %*% th)
        p <- p / sum(p)
        p <- pmax(p, ep)
        p <- p / sum(p)
        P[, iu] <- p
      }
      LL <- LL + log(p[y[iu]])
      Op <- diag(as.vector(p)) - p %*% t(p)
      R <- Co %*% diagv(1 / (Ma %*% p), Ma) %*% Op %*% G # Check
      while (rcond(R) < 0.000000000001) {
        R <- R + diag(seta)
      }
      R <- solve(R)
      eta <- Co %*% log(Ma %*% p)
      delta <- X %*% be - eta
      dis <- dis + sum(abs(delta))
      A <- G %*% R %*% X
      B <- t(R) %*% t(G) %*% Op %*% A
      S <- S + t(B) %*% X

      #    attivare una delle due

      s <- s + (t(A[y[iu], , drop = FALSE]) - t(A) %*% p) + t(B) %*% eta # versione 1
      #     s = s +( t(A[y[iu],]) - t(A)%*% p) # versione 2
    }

    while (rcond(S) < 0.0000000001) {
      S <- S + mean(abs(diag(S))) * diag(k)
    }
    #  attivare 1 delle due

    b0 <- be
    v <- solve(S, s) #  version 1
    #    b0=be; v = b0 + solve(S) %*% s # version 2

    if (is.null(rc) & is.null(rd)) {
      de <- v - b0
    } else if (is.null(rc)) { # only inequalities
      Si <- solve(S)
      Li <- t(chol(Si))
      Di <- D %*% Li
      de <- NULL
      # de = v - b0 + Li %*% ldp(Di,-D %*% v) # Needs ldp
    } else if (is.null(rd)) { # only equalities
      Ai <- solve(t(U) %*% S %*% U)
      de <- U %*% Ai %*% t(U) %*% S %*% v - b0
    } else { # both  equalities and inequalities
      Ai <- solve(t(U) %*% S %*% U)
      Li <- t(chol(Ai))
      Dz <- D %*% U
      ta <- Ai %*% t(U) %*% S %*% v
      # de = U %*% (ta + Li %*% ldp(Dz %*% Li, -Dz %*% ta)) - b0 # Needs ldp
      de <- NULL
    }
    dm0 <- dm
    dm <- max(de) - min(de) # shorten step
    dd <- (dm > 1.5)
    de <- de / (1 + dd * (dm^(0.85)))
    be <- b0 + de
    diss <- sum(abs(de))
    LL0 <- LL
    it <- it + 1
    cat(c(it, LL / 100, dis / n, diss), "\n")
    #   cat(t(be), "\n")
  }
  list(LL = LL, beta = be, S = solve(S), P = P)
}



blodiag <- function(x, blo) {
  # Split a vector x into a block diagonal matrix bith components blo.
  k <- length(blo)
  B <- matrix(0, k, sum(blo))
  u <- cumsum(c(1, blo))
  for (i in 1:k) {
    sub <- u[i]:(u[i + 1] - 1)
    if(blo[i] == 0) {
      B[i, ] <- 0
    }
    else {
      B[i, sub] <- x[sub]
    }
  }
  B
}






