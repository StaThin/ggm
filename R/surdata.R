#' A simulated data set
#' 
#' Simulated data following a seemingly unrelated regression model.
#' 
#' 
#' @name surdata
#' @docType data
#' @format A data frame with 600 observations on the following 4 variables.
#' \describe{ 
#' \item{A}{a numeric response vector} 
#' \item{B}{a numeric response vector} 
#' \item{X}{a numeric vector}
#' \item{Z}{a numeric vector with codes \code{1} and \code{-1} for a binary variable} 
#' }
#' @keywords datasets
#' @examples
#' 
#' data(surdata)
#' pairs(surdata)
#' 
"surdata"