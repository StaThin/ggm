#' Graph to adjacency matrix
#' 
#' \code{grMAT} generates the associated adjacency matrix to a given graph.
#' 
#' @param agr A graph that can be a \code{graphNEL} or an
#' \code{\link[igraph]{igraph}} object or a vector of length \eqn{3e}, where
#' \eqn{e} is the number of edges of the graph, that is a sequence of triples
#' (type, node1label, node2label). The type of edge can be \code{"a"} (arrows
#' from node1 to node2), \code{"b"} (arcs), and \code{"l"} (lines).
#' @return A matrix that consists 4 different integers as an \eqn{ij}-element:
#' 0 for a missing edge between \eqn{i} and \eqn{j}, 1 for an arrow from
#' \eqn{i} to \eqn{j}, 10 for a full line between \eqn{i} and \eqn{j}, and 100
#' for a bi-directed arrow between \eqn{i} and \eqn{j}. These numbers are added
#' to be associated with multiple edges of different types. The matrix is
#' symmetric w.r.t full lines and bi-directed arrows.
#' @author Kayvan Sadeghi
#' @keywords graphs adjacency matrix mixed graph vector
#' @examples
#' 
#' ## Generating the adjacency matrix from a vector
#' exvec <-c ('b',1,2,'b',1,14,'a',9,8,'l',9,11,'a',10,8,
#'            'a',11,2,'a',11,10,'a',12,1,'b',12,14,'a',13,10,'a',13,12)
#' grMAT(exvec)
#' 
#' @export
grMAT <- function(agr) {
  if (inherits(agr, "graphNEL")) {
    agr <- igraph.from.graphNEL(agr)
  }
  if (inherits(agr, "igraph")) {
    return(get.adjacency(agr, sparse = FALSE))
  }
  if (is.character(agr)) {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form")
    }
    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]
    bn <- numeric(length(b))
    
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
    
    # Sostituzione di RR con le funzioni native di R
    Ragr <- sort(unique(agrn))
    ma <- length(Ragr)
    mat <- matrix(0, ma, ma)
    
    for (i in seq(1, length(agrn), 2)) {
      idx_i <- SPl(Ragr, agrn[i])
      idx_next <- SPl(Ragr, agrn[i + 1])
      edge_type <- bn[(i + 1) / 2]
      
      if ((edge_type == 1 && mat[idx_i, idx_next] %% 10 != 1) || 
          (edge_type == 10 && mat[idx_i, idx_next] %% 100 < 10) || 
          (edge_type == 100 && mat[idx_i, idx_next] < 100)) {
        
        mat[idx_i, idx_next] <- mat[idx_i, idx_next] + edge_type
        if (edge_type == 10 || edge_type == 100) {
          mat[idx_next, idx_i] <- mat[idx_next, idx_i] + edge_type
        }
      }
    }
    rownames(mat) <- Ragr
    colnames(mat) <- Ragr
    return(mat)
  }
}
