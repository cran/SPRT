\name{SPRT.default}
\alias{SPRT.default}
\alias{SPRT}
\title{
Wald's Sequential Probability Ratio Test
}
\description{
Perform Wald's Sequential Probability Ratio Test on variables with a Normal, Bernoulli, Exponential or Poisson distribution. Returns an object with support for \code{print} and \code{plot} methods.
}
\usage{
\method{SPRT}{default}(distribution = "bernoulli", type1 = 0.05, type2 = 0.2, h0, h1,
values = NULL, n = NULL, k = NULL)
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
  \item{values}{
an optional vector containing values of the random variable. A logical vector when distribution is \code{"bernoulli"} and a numerical vector otherwise.
}
  \item{n}{
an optional numerical scalar for the number of observations of the random variable. \code{n} is optional and can be used as an alternative to \code{values}.
}
  \item{k}{
an optional numerical scalar for the cumulative sum of the random variable. \code{k} is optional and can be used as an alternative to \code{values}.
}
}
\details{
Perform Wald's Sequential Probability Test on a simple hypothesis test of the null against the alternative.

The null hypothesis tested is that the expected value of the random variable is equal to \code{h0}. The alternative hypothesis tested is that the expected value of the random variable is equal to \code{h1}.

The expected value of the variable is the probability of success of a Bernoulli variable, the mean of a Normal variable, the mean (as well as the variance) of a Poisson variable, and the mean (as well as the standard deviation and scale parameter) of an Exponential distribution.

Optionally, specify \code{values}, a vector with observations of the random variable in the order in which they occurred. \code{values} is a logical vector of TRUE or FALSE observations in the case of a Bernoulli variable, and a numerical vector otherwise.

Or specify \code{n} and \code{k} as an alternative to \code{values}. \code{n} is the number of observations, and \code{k} is the cumulative sum of the random variable across observations (or the number of successes in the case of a Bernoulli variable). When \code{values} is given, SPRT infers \code{n} and \code{k}, and removes any \code{NA} values in the process.
}
\value{
A list with class \code{"SPRT"} containing the following components:
  \item{distribution}{
equal to \code{distribution}.}
  \item{n}{
equal to \code{n} if given, or the length of the \code{values} vector otherwise.}
  \item{k}{
equal to \code{k} if given, or the cumulative sum of the \code{values} vector otherwise.}
  \item{h0}{
equal to \code{h0}.}
  \item{h1}{
equal to \code{h1}.}
  \item{wald.A}{
the natural logarithm of Wald's A boundary (see \code{\link{waldBoundary}}).}
  \item{wald.B}{
the natural logarithm of Wald's B boundary (see \code{\link{waldBoundary}}).}
  \item{k.boundaries}{
a numerical 2x2 matrix with the slope and intercept of H0 and H1 acceptance regions under the null and alternative hypotheses.}
  \item{llr.coefficients}{
a numerical vector of the \code{n} and \code{k} coefficients behind the random variable's log-likelihood ratio.}
  \item{llr}{
a numerical scalar of the random variable's log-likelihood ratio.}
  \item{decision}{
the outcome of the Sequential Probability Ratio Test. Returns \code{FALSE} when \code{llr} >= \code{wald.A}, \code{TRUE} when \code{llr} <= \code{wald.B} and \code{NA} otherwise.}
  \item{interpretation}{
a character vector interpreting the outcome of the Sequential Probability Ratio Test. Returns \code{"Accept H1"} when \code{decision} is \code{FALSE}, \code{"Accept H0"} when \code{decision} is \code{TRUE} and \code{"Continue testing"} otherwise.}
  \item{data.llr}{
a data frame comparing the random variable's log-likelihood ratio against the natural logarithm of Wald's A and B boundaries. The data frame's columns are named \code{n}, \code{values}, \code{k}, \code{wald.B}, \code{wald.A} and \code{llr}. It contains a row for every observation of the random variable. This output is only available when you specify \code{values}.}
  \item{data.sum}{
a data frame comparing the random variable's cumulative sum, \code{k}, against acceptance boundaries for the null and alternative hypotheses. The data frame's columns are named \code{n}, \code{values}, \code{k}, \code{h0} and \code{h1}. It contains a row for every observation of the random variable. This output is only available when you specify \code{values}.}
  \item{llr.fn}{
a function that returns the value of the random variable's log-likelihood ratio for different \code{n} and \code{k}. This function is a closure; it encapsulates your \code{distribution}, \code{type1}, \code{type2}, \code{h0} and \code{h1} settings.}
  \item{h0.fn}{
a function that returns the k acceptance boundary for the null hypothesis for different \code{n}. This function is a closure; it encapsulates your \code{distribution}, \code{type1}, \code{type2}, \code{h0} and \code{h1} settings.}
  \item{h1.fn}{
a function that returns the k acceptance boundary for the alternative hypothesis for different \code{n}. This function is a closure; it encapsulates your \code{distribution}, \code{type1}, \code{type2}, \code{h0} and \code{h1} settings.}
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\author{
Stephane Mikael Bottine
}
\note{
This function returns an object with support for \code{print} and \code{plot} methods.
}
\seealso{
\code{\link{plot.SPRT}}, \code{\link{print.SPRT}}, \code{\link{waldBoundary}}
}
\examples{
# SPRT on a normally distributed random variable
set.seed(123)
test <- SPRT(distribution = "normal", type1 = 0.05, type2 = 0.20,
  h0 = 0, h1 = 1, values = rnorm(10))

# Test outcome
test

# Cumulative sum of the random variable vs H0 and H1 boundaries
test$data.sum
plot(test)

# Sequential log-likelihood ratio vs Wald's A and B constants
test$data.llr
plot(test, log = "y")

# Calculate the log-likelihood ratio across scalars or vectors
set.seed(123)
test$llr.fn(n = 10, sum(rnorm(10)))

set.seed(123)
test$llr.fn(n = seq(1,10,1), k = cumsum(rnorm(10)))

# Calculate H0 and H1 boundaries
test$h0.fn(n = seq(1,10,1))
test$h1.fn(n = seq(1,10,1))
}