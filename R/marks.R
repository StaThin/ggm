#' Mathematics marks
#' 
#' Examination marks of 88 students in five subjects.
#' 
#' Mechanics and Vectors were closed book examinations. Algebra, Analysis and
#' Statistics were open book examinations.
#' 
#' @name marks
#' @docType data
#' @format A data frame with 88 observations on the following 5 variables.
#' \describe{ \item{mechanics}{a numeric vector, mark in Mechanics}
#' \item{vectors}{a numeric vector, mark in Vectors} \item{algebra}{a numeric
#' vector, mark in Algebra} \item{analysis}{a numeric vector, mark in Analysis}
#' \item{statistics}{a numeric vector, mark in Statistics } }
#' @references Whittaker, J. (1990). \emph{Graphical models in applied
#' multivariate statistics}. Chichester: Wiley.
#' @source Mardia, K.V., Kent, J.T. and Bibby, J.M. (1979). \emph{Multivariate
#' analysis}. London: Academic Press.
#' @keywords datasets
#' @examples
#' 
#' data(marks)
#' pairs(marks)
#' 
"marks"