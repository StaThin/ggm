#' Anger data
#'
#' Anger data
#'
#' Trait variables are viewed as stable personality characteristics, and state
#' variables denote behaviour in specific situations.  See Cox and Wermuth
#' (1996).
#'
#' @name anger
#' @docType data
#' @format A covariance matrix for 4 variables measured on 684 female students.
#' \describe{ \item{X}{anxiety state} \item{Y}{anger state} \item{Z}{anxiety
#' trait} \item{U}{anger trait} }
#' @references Cox, D. R. and Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman and Hall.
#'
#' Cox, D.R. and Wermuth, N. (1990). \emph{An approximation to maximum
#' likelihood estimates in reduced models}. 77(4), 747-761.
#' @keywords datasets
#' @examples
#'
#' # Fit a chordless 4-cycle model
#' data(anger)
#' G = UG(~ Y*X + X*Z + Z*U + U*Y)
#' fitConGraph(G,anger, 684)
#'
"anger"


#' Data on blood pressure body mass and age
#'
#' Raw data on blood pressure, body mass and age on 44 female patients, and
#' covariance matrix for derived variables.
#'
#'
#' @name derived
#' @docType data
#' @format A list containing a dataframe \code{raw} with 44 lines and 5 columns
#' and a symmetric 4x4 covariance matrix \code{S}.
#'
#' The following is the description of the variables in the dataframe
#' \code{raw} \describe{ \item{list("Sys")}{Systolic blood pressure, in mm Hg}
#' \item{list("Dia")}{Diastolic blood pressure, in mm Hg}
#' \item{list("Age")}{Age of the patient, in years} \item{list("Hei")}{Height,
#' in cm} \item{list("Wei")}{Weight, in kg} } The following is the description
#' of the variables for the covariance matrix \code{S}.  \describe{
#' \item{list("Y")}{Derived variable \code{Y=log(Sys/Dia)}}
#' \item{list("X")}{Derived variables \code{X=log(Dia)}} \item{list("Z")}{Body
#' mass index \code{Z=Wei/(Hei/100)^2}} \item{list("W")}{Age} }
#' @references Wermuth N. and Cox D.R. (1995). Derived variables calculated
#' from similar joint responses: some characteristics and examples.
#' \emph{Computational Statistics and Data Analysis}, 19, 223-234.
#' @keywords datasets
#' @examples
#'
#' # A DAG model with a latent variable U
#' G = DAG(Y ~ Z + U, X ~ U + W, Z ~ W)
#'
#' data(derived)
#'
#' # The model fitted using the derived variables
#' out = fitDagLatent(G, derived$S, n = 44, latent = "U")
#'
#' # An ancestral graph model marginalizing over U
#' H = AG(G, M = "U")
#'
#' # The ancestral graph model fitted obtaining the
#' # same result
#' out2 = fitAncestralGraph(H, derived$S, n = 44)
#'
"derived"

#' Glucose control
#'
#' Data on glucose control of diabetes patients.
#'
#' Data on 68 patients with fewer than 25 years of diabetes. They were
#' collected at the University of Mainz to identify psychological and
#' socio-economic variables possibly important for glucose control, when
#' patients choose the appropriate dose of treatment depending on the level of
#' blood glucose measured several times per day.
#'
#' The variable of primary interest is \code{Y}, glucose control, measured by
#' glycosylated haemoglobin. \code{X}, knowledge about the illness, is a
#' response of secondary interest. Variables \code{Z}, \code{U} and \code{V}
#' measure patients' type of attribution, called fatalistic externality, social
#' externality and internality. These are intermediate variables. Background
#' variables are \code{W}, the duration of the illness, \code{A} the duration
#' of formal schooling and \code{B}, gender. The background variables \code{A}
#' and \code{B} are binary variables with coding \code{-1}, \code{1}.
#'
#' @name glucose
#' @docType data
#' @format A data frame with 68 observations on the following 8 variables.
#' \describe{ \item{Y}{a numeric vector, Glucose control (glycosylated
#' haemoglobin), values up to about 7 or 8 indicate good glucose control.}
#' \item{X}{a numeric vector, a score for knowledge about the illness.}
#' \item{Z}{a numeric vector, a score for fatalistic externality (mere chance
#' determines what occurs).} \item{U}{a numeric vector, a score for social
#' externality (powerful others are responsible).} \item{V}{a numeric vector, a
#' score for internality (the patient is him or herself responsible).}
#' \item{W}{a numeric vector, duration of the illness in years.} \item{A}{a
#' numeric vector, level of education, with levels \code{-1}: at least 13 years
#' of formal schooling, \code{1}: less then 13 years.} \item{B}{a numeric
#' vector, gender with levels \code{-1}: females, \code{1}: males.} }
#' @references Cox, D. R. & Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman & Hall.
#' @source Cox & Wermuth (1996), p. 229.
#' @keywords datasets
#' @examples
#'
#' data(glucose)
#' ## See Cox & Wermuth (1996), Figure 6.3 p. 140
#' coplot(Y ~ W | A, data=glucose)
#'
"glucose"

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
#' @source Mardia, K.V., Kent, J.T. and Bibby, (1979). \emph{Multivariate
#' analysis}. London: Academic Press.
#' @keywords datasets
#' @examples
#'
#' data(marks)
#' pairs(marks)
#'
"marks"



#' Stress
#'
#' Stress data
#'
#' See Cox and Wermuth (1996).
#'
#' @name stress
#' @docType data
#' @format A \eqn{4 \times 4} covariance matrix for the following variables.
#' \describe{ \item{Y}{} \item{V}{} \item{X}{} \item{U}{} }
#' @references Cox, D. R. & Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman & Hall.
#'
#' Slangen K., Kleemann P.P and Krohne H.W. (1993). Coping with surgical
#' stress. In: Krohne H. W. (ed.).  \emph{Attention and avoidance: Strategies
#' in coping with aversiveness}. New York, Heidelberg: Springer, 321-346.
#' @keywords datasets
#' @examples
#'
#' data(stress)
#' G = UG(~ Y*X + X*V + V*U + U*Y)
#' fitConGraph(G, stress, 100)
#'
"stress"


#' A simulated data set
#'
#' Simulated data following a seemingly unrelated regression model.
#'
#'
#' @name surdata
#' @docType data
#' @format A data frame with 600 observations on the following 4 variables.
#' \describe{ \item{list("A")}{a numeric response vector} \item{list("B")}{a
#' numeric response vector} \item{list("X")}{a numeric vector}
#' \item{list("Z")}{a numeric vector with codes \code{1} and \code{-1} for a
#' binary variables.} }
#' @keywords datasets
#' @examples
#'
#' data(surdata)
#' pairs(surdata)
#'
"surdata"




