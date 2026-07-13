#' Find element positions in a sorted vector
#' 
#' Sorts the input vector \code{a} and returns the indices of the 
#' positions where the elements contained in \code{b} are found.
#' 
#' @param a A vector (character or numeric) to be sorted and analyzed.
#' @param b A vector containing the elements to search for.
#' 
#' @return An integer vector representing the indices of the matches within the sorted vector.
#' @author Kayvan Sadeghi, Giovanni M. Marchetti
#' @seealso \code{\link{sort}}, \code{\link[base]{%in%}}, \code{\link{which}}
#' @export
SPl <- function(a, b) {
  which(sort(a) %in% b)
}
#' Deviance of a Gaussian graphical model
#' 
#' Computes the deviance of a Gaussian model given the concentration matrix, 
#' the sample covariance matrix, the sample size, and the number of variables.
#' 
#' @param K A square concentration matrix (inverse covariance matrix).
#' @param S A square sample covariance matrix.
#' @param n An integer indicating the sample size.
#' @param k An integer indicating the number of variables (number of rows of \code{S}).
#' 
#' @return A numeric value representing the deviance of the Gaussian model.
#' @author Kayvan Sadeghi, Giovanni M. Marchetti
#' @seealso \code{\link{log}}, \code{\link{det}}, \code{\link{diag}}
#' @export
likGau <- function(K, S, n, k) {
  # deviance of the Gaussian model.
  SK <- S %*% K
  tr <- function(A) sum(diag(A))
  (tr(SK) - log(det(SK)) - k) * n
}
