\name{boundary.fn}
\alias{boundary.fn}
\title{
boundary.fn internal function
}
\description{
Return a numerical 2x2 matrix with the slope and intercept of H0 and H1 acceptance regions under the null and alternative hypotheses.
}
\usage{
boundary.fn(distribution = "bernoulli", type1 = 0.05, type2 = 0.2, h0, h1)
}
\arguments{
  \item{distribution}{
a character string specifying the distribution. Must be one of \code{"bernoulli"} (default), \code{"normal"}, \code{"exponential"} or \code{"poisson"}.
}
  \item{type1}{
the type I error. A number between 0 and 1.
}
  \item{type2}{
the type II error. A number between 0 and 1.
}
  \item{h0}{
the expected value of the random variable under the null hypothesis.
}
  \item{h1}{
the expected value of the random variable under the alternative hypothesis.
}
}
\details{
 This function is an internal function and need not be accessed directly. Its output is available directly from \code{\link{SPRT}} under the \code{k.boundaries} name.
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