\name{llr.fn}
\alias{llr.fn}
\title{
LLR internal function
}
\description{
The log-likelihood ratio can be expressed as a function of \code{k} and \code{n}. This internal function returns a numeric vector with the coefficients of \code{k} and \code{n}.
}
\usage{
llr.fn(distribution, h0, h1)
}
\arguments{
  \item{distribution}{
a character string specifying the distribution. Must be one of \code{"bernoulli"} (default), \code{"normal"}, \code{"exponential"} or \code{"poisson"}.
}
  \item{h0}{
the expected value of the random variable under the null hypothesis.
}
  \item{h1}{
the expected value of the random variable under the alternative hypothesis.
}
}
\details{
 This function is an internal function and need not be accessed directly. Its output is available directly from \code{\link{SPRT}} under the \code{llr.coefficients} name.
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