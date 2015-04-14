\name{SPRT-package}
\alias{SPRT-package}
\docType{package}
\title{
The SPRT package
}
\description{
Perform Wald's Sequential Probability Ratio Test on variables with a Normal, Bernoulli, Exponential and Poisson distribution. Plot acceptance and continuation regions, or create your own with the help of closures.
}
\details{
\tabular{ll}{
Package: \tab SPRT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-13\cr
License: \tab BSD_3_clause + file LICENSE\cr
}
}
\author{
Stephane Mikael Bottine
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\keyword{package}
\examples{
# SPRT on a normally distributed random variable
set.seed(123)
test <- SPRT(distribution = "normal", type1 = 0.05, type2 = 0.20,
	h0 = 0, h1 = 1, values = rnorm(10))

# Test outcome
test

# Cum. sum of the random variable vs H0 and H1 boundaries
test$data.sum
plot(test)

# Log-likelihood ratio vs Wald's A and B boundaries
test$data.llr
plot(test, log = "y")

# Log-likelihood ratio across scalars or vectors
set.seed(123)
test$llr.fn(n = 10, sum(rnorm(10)))

set.seed(123)
test$llr.fn(n = seq(1,10,1), k = cumsum(rnorm(10)))

# H0 and H1 boundaries
test$h0.fn(n = seq(1,10,1))
test$h1.fn(n = seq(1,10,1))
}
