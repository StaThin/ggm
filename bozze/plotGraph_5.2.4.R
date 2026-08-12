#' Plot of a mixed graph
#' 
#' Plots a mixed graph from an adjacency matrix, a `graphNEL` 
#' object, an `igraph` object, or a descriptive vector.
#' 
#' `plotGraph` uses [igraph::plot.igraph()] and
#' [igraph::tkplot()].
#' 
#' @param a An adjacency matrix: a matrix that consists of 4 different integers
#' as an \eqn{ij}-element: 0 for a missing edge between \eqn{i} and \eqn{j}, 1
#' for an arrow from \eqn{i} to \eqn{j}, 10 for a full line between \eqn{i} and
#' \eqn{j}, and 100 for a bi-directed arrow between \eqn{i} and \eqn{j}. These
#' numbers can be added to generate multiple edges of different types. The
#' matrix must be symmetric w.r.t full lines and bi-directed arrows. Or a graph
#' that can be a \code{graphNEL} or an \code{\link[igraph]{igraph}} object.Or a
#' vector of length \eqn{3e}, where \eqn{e} is the number of edges of the
#' graph, that is a sequence of triples (type,node1label,node2label). The type
#' of edge can be \code{"a"} (arrows from node1 to node2), \code{"b"} (arcs),
#' and \code{"l"} (lines).
#' @param dashed A logical value. If \code{TRUE} the bi-directed edges are
#' plotted as undirected dashed edges.
#' @param tcltk A logical value. If \code{TRUE} the function opens a tcltk
#' device to plot the graphs, allowing the interactive manimulation of the
#' graph. If \code{FALSE}the function opens a standard device without
#' interaction.
#' @param layout The name of a function used to compute the (initial) layout of
#' the graph. The default is \code{layout.auto}. This can be further adjusted
#' if \code{tcltk} is \code{TRUE}.
#' @param directed A logical value. If \code{FALSE} a symmetric adjacency
#' matrix with entries 1 is interpreted as an undirected graph. If \code{TRUE}
#' it is interpreted as a directed graph with double arrows. If \code{a} is not
#' an adjacency matrix, it is ignored.
#' @param noframe A logical value. If \code{TRUE}, then the nodes are not
#' circled.
#' @param nodesize An integer denoting the size of the nodes (default 15). It
#' can be increased to accommodate larger labels.
#' @param vld An integer defining the distance between a vertex and its label.
#' Defaults to 0.
#' @param vc Vertex color. Default is "gray".
#' @param vfc Vertex frame color. Default is "black".
#' @param colbid Color of the bi-directed edges. Default is "FireBrick3".
#' @param coloth Color of all the other edges. Default is "black".
#' @param cex An integer (defaults to 1) to adjust the scaling of the font of
#' the labels.
#' @param \dots Further arguments to be passed to \code{plot} or \code{tkplot}.
#' @return Plot of the associated graph and returns invisibly a list with two
#' slots: \code{tkp.id}, \code{graph}, the input graph as an \code{igraph}
#' object. The id can be used to get the layout of the adjusted graph. The
#' bi-directed edges are plotted in red.
#' @author Kayvan Sadeghi, Giovanni M. Marchetti
#' @seealso \code{\link{grMAT}}, \code{\link[igraph]{tkplot}},
#' \code{\link{drawGraph}}, \code{\link[igraph]{plot.igraph}}
#' @keywords graphs adjacency matrix mixed graphs plot
#' @export plotGraph
plotGraph <- function(a, 
                      dashed = FALSE, 
                      tcltk = FALSE, 
                      layout = igraph::layout_nicely, 
                      directed = FALSE, 
                      noframe = FALSE, 
                      nodesize = 15, 
                      vld = 0, 
                      vc = "gray", 
                      vfc = "black", 
                      colbid = "FireBrick3", 
                      coloth = "black", 
                      cex = 1.5, ...) {
  if (class(a)[1] == "igraph" || class(a)[1] == "graphNEL" || class(a)[1] ==
      "character") {
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
    } else {
      stop("'object' is not in a valid adjacency matrix form")
    }
    if (length(l1) > 0) {
      ## l1 <- l1 - 1   # igraph0
      agr <- graph(l1, n = nrow(a), directed = TRUE)
    }
    if (length(l1) == 0) {
      agr <- graph.empty(n = nrow(a), directed = TRUE)
      return(tkplot(agr, vertex.label = rownames(a)))
    }
    ed0 <- get.edgelist(agr)
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
          } else {
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
    if (dashed) {
      ety <- rep(1, ne)
      ety[l2 == 3] <- 2
      l2[l2 == 3] <- 0
    } else {
      ety <- rep(1, ne)
    }
    if (noframe) {
      vfc <- "white"
      vc <- "white"
    }
    if (tcltk == TRUE) {
      id <- tkplot(agr,
                   layout = layout, edge.curved = curve,
                   vertex.label = rownames(a), edge.arrow.mode = l2,
                   edge.color = col, edge.lty = ety,
                   vertex.label.family = "sans",
                   edge.width = 1.5, vertex.size = nodesize,
                   vertex.frame.color = vfc, vertex.color = vc,
                   vertex.label.cex = cex, edge.arrow.width = 1,
                   edge.arrow.size = 1.2, vertex.label.dist = vld, ...
      )
    } else {
      id <- plot(agr,
                 layout = layout, edge.curved = curve,
                 vertex.label = rownames(a), edge.arrow.mode = l2,
                 edge.color = col, edge.lty = ety,
                 vertex.label.family = "sans",
                 edge.width = 2, vertex.size = nodesize * 1.5,
                 vertex.frame.color = vfc, vertex.color = vc,
                 vertex.label.cex = cex * 0.8, edge.arrow.width = 2,
                 edge.arrow.size = .5, vertex.label.dist = vld, ...
      )
    }
    V(agr)$name <- rownames(a)
    agr <- set.edge.attribute(agr, "edge.arrow.mode", index = E(agr), l2)
    return(invisible(list(tkp.id = id, igraph = agr)))
  } else {
    stop("'object' is not in a valid format")
  }
}

#' Graph to adjacency matrix
#'
#' `grMAT` converts graph objects to a mixed adjacency matrix.
#'
#' @details Support for `graphNEL` objects requires the `graph` package 
#'   from Bioconductor, which is a suggested dependency. 
#'   If the package is missing, passing a `graphNEL` object will 
#'   trigger an informative error.
#'
#' @param agr A graph object. This can be:
#' * a `graphNEL` object, 
#' * an [igraph::igraph()] object, or 
#' * a character vector of length \eqn{3e}{3e}, where \eqn{e} 
#'   is the number of edges. If it is a vector, it must 
#'   be a sequence of triples (`type`, `node1label`, `node2label`).
#'   
#'   The type of edge can be:
#'   * `"a"` (arrow from `node1` to `node2`),
#'   * `"b"` (bi-directed arc), 
#'   * `"l"` (undirected line), or 
#'   * `"*"` (for an isolated node with `node1 == node2`).
#'
#' @return A matrix consisting of 4 different integers 
#' representing the \eqn{ij}{ij}-elements:
#' * 0 for a missing edge between \eqn{i} and \eqn{j}, 
#' * 1 for an arrow from \eqn{i} to \eqn{j}, 
#' * 10 for a full line between \eqn{i} and \eqn{j}, and 
#' * 100 for a bi-directed arrow between \eqn{i} and \eqn{j}. 
#' These numbers are added when multiple edges of different types 
#' are present. The matrix is symmetric with respect to full lines 
#' and bi-directed arrows.
#' 
#' @author Kayvan Sadeghi, Giovanni Marchetti 
#' @keywords graphs adjacency matrix mixed-graph vector
#' 
#' @examples
#' ## Generating the adjacency matrix from a vector
#' exvec <- c(
#'   "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11, "a", 10, 8,
#'   "a", 11, 2, "a", 11, 10, "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12)
#' grMAT(exvec)
#'
#' \dontrun{
#' ## Example with graphNEL (requires the 'graph' package from Bioconductor)
#' if (requireNamespace("graph", quietly = TRUE)) {
#'   V <- c("a", "b", "c")
#'   g <- graph::graphNEL(nodes = V, edgemode = "undirected")
#'   grMAT(g)
#'  }
#' }
#' 
#' g <- c("a", 3, 1, "l", 1, 2, "*", 4, 4)
#' grMAT(g)
#' @export
grMAT <- function(agr) {
  # 1. Safe check for graphNEL objects (S4 compatible for Bioconductor)
  if ("graphNEL" %in% class(agr) || inherits(agr, "graph")) {
    if (!requireNamespace("graph", quietly = TRUE)) {
      stop(
        "Package 'graph' (Bioconductor) is required to convert graphNEL objects.\n", 
        "It can be installed using: BiocManager::install('graph')", 
        call. = FALSE
      )
    }
    agr <- methods::as(agr, "matrix")
    return(agr) 
  }
  
  
  # 2. Safe check for igraph objects (using direct namespace call)
  if (inherits(agr, "igraph")) {
    return(igraph::as_adjacency_matrix(agr, sparse = FALSE))
  }
  
  # 3. Processing character vectors representing mixed graphs
  if (inherits(agr, "character")) {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form", call. = FALSE)
    }
    
    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]
    
    # Updated check: Now allowing '*' to declare isolated nodes
    if (!all(b %in% c("a", "l", "b", "*"))) {
      stop("'The numeric object' is not in a valid form", call. = FALSE)
    }
    
    # Sort unique nodes once at the beginning (includes isolated nodes defined via '*')
    Ragr <- sort(unique(agrn)) 
    ma <- length(Ragr)
    mat <- matrix(0, nrow = ma, ncol = ma)
    rownames(mat) <- Ragr
    colnames(mat) <- Ragr
    
    # Vectorized assignment of edge weights (isolated placeholder '*' gets weight 0)
    bn <- numeric(length(b))
    bn[b == "l"] <- 10
    bn[b == "a"] <- 1
    bn[b == "b"] <- 100
    bn[b == "*"] <- 0
    
    # Extract all source and destination nodes at once
    origins <- agrn[seq(1, length(agrn), 2)]
    destinations <- agrn[seq(2, length(agrn), 2)]
    
    # Match node positions instantly without any SPl helper function
    row_indices <- match(origins, Ragr)
    col_indices <- match(destinations, Ragr)
    
    # Loop to accumulate single and multiple edge weights
    for (i in seq_along(bn)) {
      r <- row_indices[i]
      c <- col_indices[i]
      val_bn <- bn[i]
      
      # If it's a designated isolated node marker or an explicit self-loop placeholder,
      # do nothing and skip to the next iteration to keep the diagonal at 0.
      if (b[i] == "*" || r == c) {
        next
      }
      
      current_val <- mat[r, c]
      
      # Historical checks for modulo remainders to accumulate overlapping edges
      if ((val_bn == 1   && current_val %% 10 != 1) || 
          (val_bn == 10  && current_val %% 100 < 10) || 
          (val_bn == 100 && current_val < 100)) {
        
        mat[r, c] <- current_val + val_bn
        
        # Apply symmetry for undirected ('l') and bi-directed ('b') edges
        if (val_bn == 10 || val_bn == 100) {
          mat[c, r] <- mat[c, r] + val_bn
        }
      }
    }
    return(mat)
  }
  
  # 4. Fallback for unsupported object classes
  stop("Input 'agr' must be a graphNEL, igraph, or character object.", call. = FALSE)
}
