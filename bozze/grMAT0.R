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
