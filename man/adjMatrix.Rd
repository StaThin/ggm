% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{adjMatrix}
\alias{adjMatrix}
\title{Adjacency matrix of a graph}
\usage{
adjMatrix(A)
}
\arguments{
\item{A}{a square matrix representing the edge matrix of a graph.}
}
\value{
\item{E}{the adjacency matrix of the graph.}
}
\description{
Transforms the ``edge matrix'' of a graph into the adjacency matrix.
}
\details{
Given the edge matrix \eqn{A} of a graph, this can be transformed into an
adjacency matrix \eqn{E} with the formula \eqn{E = (A-I)^T}.
}
\examples{

amat <- DAG(y ~ x+z, z~u+v)
E <- edgematrix(amat)
adjMatrix(E)

}
\seealso{
\code{\link{edgematrix}}
}
\author{
Giovanni M. Marchetti
}
\keyword{algebra}
\keyword{array}
\keyword{graphs}
\keyword{multivariate}