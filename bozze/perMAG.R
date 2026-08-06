

#'
#' @details Support for `graphNEL` objects requires the `graph` package 
#'   from Bioconductor, which is a suggested dependency. 
#'   If the package is missing, passing a `graphNEL` object will 
#'   trigger an informative error.
#'   
#'   This function uses the internal functions [AG()] and [Max()].
#'
#' @param amat An adjacency matrix, or a graph object. This can be:
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
#' @param M A subset of the node set of `amat` that is going to be
#' marginalized over.
#' @param C Another disjoint subset of the node set of `amat` that 
#' is going to be conditioned on.
#' @param showmat A logical value. `TRUE` (by default) to print the
#' generated matrix.
#' @param plot `FALSE` (by default). `TRUE` to plot the generated graph.
#' @param plotfun Function to plot the graph when `plot = TRUE`. 
#' Can be `plotGraph` (the default) or `drawGraph`.
#' @param ... Further arguments passed to `plotfun`.
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
#' @author Kayvan Sadeghi
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#'  @details Support for `graphNEL` objects requires the `graph` package 
#'   from Bioconductor, which is a suggested dependency. 
#'   If the package is missing, passing a `graphNEL` object will 
#'   trigger an informative error.
#'
#' @param amat An adjacency matrix, or a graph object. This can be:
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
#' @param bmat The same as `amat`.
#' 
#' @return A character string: either `"Markov Equivalent"` or `"NOT Markov Equivalent"`.
#' 
#' @author Kayvan Sadeghi
#'