\name{print.SPRT}
\alias{print.SPRT}
\title{
Print SPRT boundaries and outcome
}
\description{
Returns a user-friendly summary of the test's outcome, including Wald boundaries.
}
\usage{
\method{print}{SPRT}(x = SPRT, \dots)
}
\arguments{
  \item{x}{
an object with class \code{"SPRT"}; the output of the \code{\link{SPRT}} function.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\author{
Stephane Mikael Bottine
}
\seealso{
\code{\link{print.default}}
\code{\link{SPRT}}
}