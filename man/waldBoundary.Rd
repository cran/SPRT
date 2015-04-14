\name{waldBoundary}
\alias{waldBoundary}
\title{
Wald Boundaries
}
\description{
Calculate Wald's A and B boundaries, such that A and B satisfy 0 < B < A < Inf. Note that the function returns the natural logarithm of the boundaries by default.
}
\usage{
waldBoundary(type1 = 0.05, type2 = 0.2, boundary = NULL, log = TRUE)
}
\arguments{
  \item{type1}{type I error rate.}
  \item{type2}{type II error rate.}
  \item{boundary}{an optional character string specifying the boundary. Must be one of \code{"A"} or \code{"B"}.}
  \item{log}{a logical variable indicating whether to return the boundary's natural logarithm.}
}
\value{
A numeric vector of length 1 if \code{boundary} is set to either \code{"A"} or \code{"B"}, or a numeric vector of length 2 if \code{boundary} is set to NULL.
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\author{
Stephane Mikael Bottine
}
\seealso{
\code{\link{SPRT}}
}
\examples{
## Return the logarithm of Wald's A and B boundaries
waldBoundary()

# Return Wald's A and B boundaries
waldBoundary(log = FALSE)

# Return only Wald's A boundary
waldBoundary(boundary = "A", log = FALSE)

## Boundary for different type I and II error rates
waldBoundary(type1 = 0.01, type2 = 0.01, boundary = "A", log = FALSE)
}