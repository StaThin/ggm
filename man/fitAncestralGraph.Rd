\name{fitAncestralGraph}
\alias{fitAncestralGraph}
\title{Fitting of Gaussian Ancestral Graph Models}
\description{
Iterative conditional fitting of Gaussian Ancestral Graph Models.
}
\usage{
fitAncestralGraph(amat, S, n, tol = 1e-06)
}
\arguments{
  \item{amat}{a square matrix, representing the adjacency matrix of
    an ancestral graph.}
  \item{S}{a symmetric positive definite matrix with row and col names, the
    sample covariance matrix.}
  \item{n}{the sample size, a positive integer.}
  \item{tol}{a small positive number indicating the tolerance
    used in convergence checks.}
}
\details{
 In the Gaussian case, the models can be parameterized
  using precision parameters, regression coefficients, and error
  covariances (compare Richardson and Spirtes, 2002, Section 8). This
  function finds the MLE \eqn{\hat \Lambda}{L} of the precision 
  parameters by fitting a concentration  
  graph model. The MLE \eqn{\hat B}{B} of the regression coefficients and
  the MLE \eqn{\hat\Omega}{O} of the error covariances are obtained by
  iterative conditional fitting (Drton and Richardson, 2003, 2004). The
  three sets of parameters are 
  combined to the MLE \eqn{\hat\Sigma}{S} of the covariance matrix by
  matrix multiplication: 
  \deqn{\hat\Sigma = \hat B^{-1}(\hat \Lambda+\hat\Omega)\hat
    B^{-T}.}{S = B^(-1) (L+O) B^(-t).}
  Note that in Richardson and Spirtes (2002), the matrices \eqn{\Lambda}{L}
  and \eqn{\Omega}{O} are defined as submatrices. 
}
\value{
  \item{Shat}{the fitted covariance matrix.}
  \item{Lhat}{matrix of the fitted precisions associated with undirected
    edges and vertices that do not have an arrowhead pointing at them.}
  \item{Bhat}{matrix of the fitted regression coefficients
    associated to the directed edges.  Precisely said \code{Bhat}
    contains ones on the diagonal and the off-diagonal entry
    \eqn{(i,j)}{(i,j)} equals the \emph{negated} MLE of the regression
    coefficient for variable \eqn{j}{j} in the regression of variable
    \eqn{i}{i} on its parents. Note that this \eqn{(i,j)}{(i,j)} entry
    in \code{Bhat} corresponds to a directed edge \eqn{j \to i}{j -> i},
    and thus to a one as \eqn{(j,i)}{(j,i)} entry in the adjacency matrix.}
  \item{Ohat}{matrix of the error covariances and variances of the residuals 
    between regression equations associated with bi-directed edges and
    vertices with an arrowhead pointing at them.}
  \item{dev}{the `deviance' of the model.}
  \item{df}{the degrees of freedom.}
  \item{it}{the iterations.}
}
\references{ 
  Drton, M. and Richardson, T. S. (2003). A new algorithm for
  maximum likelihood estimation in Gaussian graphical models for
  marginal independence. \emph{Proceedings
    of the Nineteenth Conference on Uncertainty in Artificial
    Intelligence}, 184-191.

  Drton, M. and Richardson, T. S. (2004). Iterative Conditional Fitting
  for Gaussian Ancestral Graph Models.  Proceedings
  of the 20th Conference on Uncertainty in Artificial Intelligence,
  Department of Statistics, 130-137.
  
  Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov
  Models. \emph{Annals of Statistics}. 30(4), 962-1030.
}
\author{Mathias Drton}
\seealso{\code{\link{fitCovGraph}}, \code{\link{icf}},
  \code{\link{makeMG}}, \code{\link{fitDag}}
  }
\examples{
## A covariance matrix
"S" <- structure(c(2.93, -1.7, 0.76, -0.06,
                  -1.7, 1.64, -0.78, 0.1,
                   0.76, -0.78, 1.66, -0.78,
                  -0.06, 0.1, -0.78, 0.81), .Dim = c(4,4),
                 .Dimnames = list(c("y", "x", "z", "u"), c("y", "x", "z", "u")))
## The following should give the same fit.   
## Fit an ancestral graph y -> x <-> z <- u
fitAncestralGraph(ag1 <- makeMG(dg=DAG(x~y,z~u), bg = UG(~x*z)), S, n=100)

## Fit an ancestral graph y <-> x <-> z <-> u
fitAncestralGraph(ag2 <- makeMG(bg= UG(~y*x+x*z+z*u)), S, n=100)

## Fit the same graph with fitCovGraph
fitCovGraph(ag2, S, n=100)    

## Another example for the mathematics marks data

data(marks)
S <- var(marks)
mag1 <- makeMG(bg=UG(~mechanics*vectors*algebra+algebra*analysis*statistics))
fitAncestralGraph(mag1, S, n=88)

mag2 <- makeMG(ug=UG(~mechanics*vectors+analysis*statistics),
               dg=DAG(algebra~mechanics+vectors+analysis+statistics))
fitAncestralGraph(mag2, S, n=88) # Same fit as above
}
\keyword{graphs}
\keyword{models} 
\keyword{ancestral graph}
\keyword{multivariate}







