
#' Stress data
#' 
#' Covariance matrix from a study on coping with surgical stress.
#' 
#' See Cox and Wermuth (1996) for details on multivariate dependencies.
#' 
#' @name stress
#' @docType data
#' @format A \eqn{4 \times 4} covariance matrix for the following variables:
#' \describe{ 
#'   \item{Y}{Stress score Y} 
#'   \item{V}{Stress score V} 
#'   \item{X}{Stress score X} 
#'   \item{U}{Stress score U} 
#' }
#' @references Cox, D. R. & Wermuth, N. (1996). \emph{Multivariate dependencies}. London: Chapman & Hall.
#' 
#' Slangen K., Kleemann P.P and Krohne H.W. (1993). Coping with surgical stress. In: Krohne H. W. (ed.). \emph{Attention and avoidance: Strategies in coping with aversiveness}. New York, Heidelberg: Springer, 321-346.
#' @keywords datasets
#' @examples
#' data(stress)
#' G <- UG(~ Y*X + X*V + V*U + U*Y)
#' fitConGraph(G, stress, 100)
"stress"
