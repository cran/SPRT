\name{C.fn}
\alias{C.fn}
\title{
C.fn internal function
}
\description{
Return a component of the log-likelood ratio (LLR).
}
\usage{
C.fn(distribution, h)
}
\arguments{
  \item{distribution}{
a character string specifying the distribution. Must be one of \code{"bernoulli"} (default), \code{"normal"}, \code{"exponential"} or \code{"poisson"}.}
  \item{h}{
an instance of either \code{h0} or \code{h1}.}
}
\details{
 This function is an internal function and need not be accessed directly.
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\author{
Stephane Mikael Bottine
}