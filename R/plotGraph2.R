`plotGraph2`  <-
  function (a,
            dashed = TRUE,
            tcltk = FALSE,
            layout = layout_nicely,
            directed = FALSE,
            noframe = FALSE,
            nodesize = 20,
            vld = 0,
            vc = "gray80", # categorical_pal(1),
            vfc = NA,
            colbid = "FireBrick3",
            coloth = "gray40",
            cex = 1.5,
            ew = 1.5,
            eas = 0.9,
            ...)
  {
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
            if (a[j, i] %% 10 == 1) {
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
      #/////////////////////
      if (length(l1) > 0) {
        agr <- make_graph(l1, n = nrow(a), directed = TRUE)
      }
      if (length(l1) == 0) {
        agr <- make_empty_graph(n = nrow(a), directed = TRUE)
        return(tkplot(agr, vertex.label = rownames(a)))
      }
      ed0 <- as_edgelist(agr)
      ne <- nrow(ed0)
      ed <- apply(apply(ed0, 1, sort), 2, paste, collapse = "-")
      tb <- table(ed)
      curve <- rep(0, ne)
      if (any(tb > 1)) {
        tb <- tb[tb > 1]
        for (i in 1:length(tb)) {
          reped <- names(tb[i]) == ed
          U <- ed0[reped, ]
          if (sum(reped) == 2) {
            ed0[reped]
            if (all(is.element(c(0, 3), l2[reped]))) {
              curve[reped] <- c(0.9, -0.9)
            }
            if (all(U[1, ] == U[2, ])) {
              curve[reped] <- c(0.6, -0.6)
            }
            else {
              curve[reped] <- c(0.6, 0.6)
            }
          }
          if (sum(reped) == 3) {
            curve[(l2 == 3) & reped] <- 0.9
            curve[(l2 == 0) & reped] <- -0.9
          }
          if (sum(reped) == 4) {
            curve[(l2 == 3) & reped] <- 0.3
            curve[(l2 == 0) & reped] <- -0.3
            curve[(l2 == 1) & reped] <- 0.9
            curve[(l2 == 2) & reped] <- 0.9
          }
        }
      }
      col <- rep(coloth, ne)
      col[l2 == 3] <- colbid
      l2[l2 == 3] <- 3 # <=====
      if (dashed) {
        ety <- rep(1, ne)
        ety[l2 == 3] <- 2
      }
      else {
        ety <- rep(1, ne)
      }
      if (noframe) {
        vfc <- "white"
        vc <- "white"
      }
      #    curve[ety == 1] <- 0  
      
      if (tcltk == TRUE) {
        id <- tkplot(
          agr,
          layout = layout,
          edge.curved = curve,
          vertex.label = rownames(a),
          edge.arrow.mode = l2,
          edge.color = col,
          edge.lty = ety,
          vertex.label.family = "sans",
          edge.width = ew,
          vertex.size = nodesize,
          vertex.frame.color = vfc,
          vertex.color = vc,
          vertex.label.cex = cex,
          edge.arrow.width = 1,
          edge.arrow.size = 1.2,
          vertex.label.dist = vld,
          ...
        )
      }
      else {
        par(mar=c(0,0,0,0)+.1)
        id <- plot(
          agr,
          layout = layout,
          edge.curved = curve,
          vertex.label = rownames(a),
          edge.arrow.mode = l2,
          edge.color = col,
          edge.lty = ety,
          vertex.label.family = "sans",
          edge.width = ew,
          vertex.size = nodesize,
          vertex.frame.color = vfc,
          vertex.color = vc,
          vertex.label.cex = cex * 0.8,
          #          edge.arrow.width = 1,
          edge.arrow.size = eas,
          vertex.label.dist = vld,
          margin = c(0,0,0,0),
          ...
        )
      }
      V(agr)$name <- rownames(a)
      agr <- set_edge_attr(agr, "edge.arrow.mode",
                           index = E(agr), l2)
      return(invisible(list(
        tkp.id = id, igraph = agr
      )))
    }
    else {
      stop("'object' is not in a valid format")
    }
  }

# /////////////////////

MATgr <- function(a, directed = FALSE){
  colbid <- "FireBrick3"
  coloth <- "gray40"
  dashed <- TRUE
  noframe <- TRUE
  
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
          if (a[j, i] %% 10 == 1) {
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
      agr <- make_graph(l1, n = nrow(a), directed = TRUE)
    }
    if (length(l1) == 0) {
      agr <- make_empty_graph(n = nrow(a), directed = TRUE)
      return(tkplot(agr, vertex.label = rownames(a)))
    }
    ed0 <- as_edgelist(agr)
    ne <- nrow(ed0)
    ed <- apply(apply(ed0, 1, sort), 2, paste, collapse = "-")
    tb <- table(ed)
    curve <- rep(0, ne)
    if (any(tb > 1)) {
      tb <- tb[tb > 1]
      for (i in 1:length(tb)) {
        reped <- names(tb[i]) == ed
        U <- ed0[reped, ]
        if (sum(reped) == 2) {
          ed0[reped]
          if (all(is.element(c(0, 3), l2[reped]))) {
            curve[reped] <- c(0.9, -0.9)
          }
          if (all(U[1, ] == U[2, ])) {
            curve[reped] <- c(0.6, -0.6)
          }
          else {
            curve[reped] <- c(0.6, 0.6)
          }
        }
        if (sum(reped) == 3) {
          curve[(l2 == 3) & reped] <- 0.9
          curve[(l2 == 0) & reped] <- -0.9
        }
        if (sum(reped) == 4) {
          curve[(l2 == 3) & reped] <- 0.3
          curve[(l2 == 0) & reped] <- -0.3
          curve[(l2 == 1) & reped] <- 0.9
          curve[(l2 == 2) & reped] <- 0.9
        }
      }
    }
    col <- rep(coloth, ne)
    col[l2 == 3] <- colbid
    l2[l2 == 3] <- 3 # <=====
    if (dashed) {
      ety <- rep(1, ne)
      ety[l2 == 3] <- 2
      l2[l2 == 3] <- 0
    }
    else {
      ety <- rep(1, ne)
    }
    #    curve[ety == 1] <- 0  
  }
  V(agr)$name <- rownames(a)
  agr <- set_edge_attr(agr, "edge.arrow.mode",
                       index = E(agr), l2)
  agr
}

