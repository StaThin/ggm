#' Graph to adjacency matrix
#'
#' \code{grMAT} converts graph objects to a mixed adjacency matrix.
#'
#' @param agr A graph that can be a `graphNEL` or an
#' [igraph::igraph] object or a vector of length \eqn{3e} \eqn{3e}, where
#' \eqn{e} is the number of edges of the graph, that is a sequence of triples
#' (type, node1label, node2label). The type of edge can be \code{"a"} (arrows
#' from node1 to node2), \code{"b"} (arcs), and \code{"l"} (lines).
#'
#' @return A matrix that consists of 4 different integers as an \eqn{ij}-element:
#' 0 for a missing edge between \eqn{i} and \eqn{j}, 1 for an arrow from
#' \eqn{i} to \eqn{j}, 10 for a full line between \eqn{i} and \eqn{j}, and 100
#' for a bi-directed arrow between \eqn{i} and \eqn{j}. These numbers are added
#' to be associated with multiple edges of different types. The matrix is
#' symmetric w.r.t. full lines and bi-directed arrows.
#' @author Kayvan Sadeghi
#' @keywords graphs adjacency matrix mixed graph vector
#' @examples
#' igraph::graph_from_literal() 
#'
#' ## Generating the adjacency matrix from a vector
#' exvec <- c(
#'   "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11, "a", 10, 8,
#'   "a", 11, 2, "a", 11, 10, "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12
#' )
#' grMAT(exvec)
#'
#' @export grMAT

`grMAT` <- function(agr) {
  if (inherits(agr, "graphNEL") || inherits(agr, "graph")) {
    if (!requireNamespace("graph", quietly = TRUE)) {
      stop("Package 'graph' (Bioconductor) is required to convert graphNEL objects.\n", "It can be installed as: BiocManager::install('graph')", call. = FALSE)
    }
    agr <- methods::as(amat, "matrix")
  }
  if (class(agr)[1] == "igraph") {
    return(as_adjacency_matrix(agr, sparse = FALSE))
  }
  if (class(agr)[1] == "character") {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form")
    }
    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]
    bn <- c()
    for (i in 1:length(b)) {
      if (b[i] != "a" && b[i] != "l" && b[i] != "b") {
        stop("'The numeric object' is not in a valid form")
      }
      if (b[i] == "l") {
        bn[i] <- 10
      }
      if (b[i] == "a") {
        bn[i] <- 1
      }
      if (b[i] == "b") {
        bn[i] <- 100
      }
    }
    Ragr <- unique(agrn)
    ma <- length(Ragr)
    mat <- matrix(rep(0, (ma)^2), ma, ma)
    for (i in seq(1, length(agrn), 2)) {
      if ((bn[(i + 1) / 2] == 1 && mat[SPl(Ragr, agrn[i]), SPl(Ragr, agrn[i + 1])] %% 10 != 1) || (bn[(i + 1) / 2] == 10 && mat[SPl(Ragr, agrn[i]), SPl(Ragr, agrn[i + 1])] %% 100 < 10) || (bn[(i + 1) / 2] == 100 && mat[SPl(Ragr, agrn[i]), SPl(Ragr, agrn[i + 1])] < 100)) {
        mat[SPl(Ragr, agrn[i]), SPl(Ragr, agrn[i + 1])] <- mat[SPl(Ragr, agrn[i]), SPl(Ragr, agrn[i + 1])] + bn[(i + 1) / 2]
        if (bn[(i + 1) / 2] == 10 || bn[(i + 1) / 2] == 100) {
          mat[SPl(Ragr, agrn[i + 1]), SPl(Ragr, agrn[i])] <- mat[SPl(Ragr, agrn[i + 1]), SPl(Ragr, agrn[i])] + bn[(i + 1) / 2]
        }
      }
    }
    rownames(mat) <- Ragr
    colnames(mat) <- Ragr
  }
  return(mat)
}


dagitty2mam <- function(dgy) {
  nodes <- names(dgy)
  n <- length(nodes)
  amat <- matrix(0, nrow = n, ncol = n, dimnames = list(nodes, nodes))
  amat.dg <- amat
  amat.ug <- amat
  amat.bg <- amat
  
  e <- dagitty::edges(dgy)
  
  if(nrow(e) == 0) return(mat)
  
  for(i in 1:nrow(e)) {
    orig <- as.character(e$v[i])
    dest <- as.character(e$w[i])
    type <- as.character(e$e[i])
    
    if (type == "->") {
      # Arrow: only a direction
      amat.dg[orig, dest] <- 1
    } else if (type == "<->") {
      # Bidirected edge: symmetric
      amat.bg[orig, dest] <- 100
      amat.bg[dest, orig] <- 100
    } else if (type == "--") {
      # Undirected edge: symmetric
      amat.ug[orig, dest] <- 10
      amat.ug[dest, orig] <- 10
    } 
  }
  # mixed adjacency matrix
  mam <- amat.dg + amat.ug + amat.bg
  return(mam)
}

################################################

`plotGraph2`  <-
  function (a,
            dashed = FALSE,
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

