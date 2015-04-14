\name{plot.SPRT}
\alias{plot.SPRT}
\title{
Plot SPRT boundaries and acceptance regions
}
\description{
Plot SPRT boundaries against the cumulative sum of the random variable (default) or the log-likelihood ratio (when you set \code{log = "y"}).
}
\usage{
\method{plot}{SPRT}(x = SPRT, y = NULL, type = NULL, xlim = NULL, ylim = NULL, log = "", 
    main = "SPRT", sub = NULL, xlab = "Observations", ylab = NULL, 
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
    panel.last = NULL, asp = NULL, col = 1, lty = 2, lwd = 1, 
    \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object with class \code{"SPRT"}; the output of the \code{\link{SPRT}} function.
}
  \item{y}{
the y coordinates of the points in the plot, automatically populated when an object with class \code{"SPRT"} is provided.
}
  \item{type}{
a 1-character string for the type of plot. The following values are possible: \code{"p"} for points (default), \code{"l"} for lines, \code{"b"} for points and lines, \code{"c"} for empty points joined by lines, \code{"o"} for overplotted points and lines, \code{"s"} and \code{"S"} for stair steps and \code{"h"} for histogram-like vertical lines. Finally, "n" hides all points.
}
  \item{xlim}{
numeric vector of length 2, giving the x coordinates ranges.
}
  \item{ylim}{
numeric vector of length 2, giving the y coordinates ranges.
}
  \item{log}{
set \code{log} equal to "y" to plot the random variable's log-likelihood ratio against the natural logarithm of Wald's A and B boundaries. Otherwise, the plot returns the cumulative sum of the random variable on the y-axis, against H1 and H0 acceptance boundaries.}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{sub}{
a sub title for the plot: see \code{\link{title}}.
}
  \item{xlab}{
a title for the x axis: see \code{\link{title}}.
}
  \item{ylab}{
a title for the y axis: see \code{\link{title}}.
}
  \item{ann}{
a logical value indicating whether to show plot and axis titles. Defaults to the current setting.
}
  \item{axes}{
a logical value indicating whether both axes should be drawn on the plot.
}
  \item{frame.plot}{
a logical indicating whether a box should be drawn around the plot.
}
  \item{panel.first}{
an 'expression' to be evaluated after the plot axes are set up but before any plotting takes place. This can be useful for drawing background grids or scatterplot smooths. Note that this works by lazy evaluation: passing this argument from other \code{plot} methods may well not work since it may be evaluated too early.
}
  \item{panel.last}{
an expression to be evaluated after plotting has taken place but before the axes, title and box are added. See the comments about \code{panel.first}.
}
  \item{asp}{
a numeric scalar giving the aspect ratio y/x.
}
  \item{col}{
a vector up to 3 elements in length. The first colour is applied to the dots, the second is applied to the H0 boundary (or B boundary if applicable) and the third is applied to the H1 boundary (or A boundary if applicable). If the vector holds 2 colours, the first colour is applied to the dots and the second is applied across boundaries.
}
  \item{lty}{
the line type. Specify the line type either as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as a character string (\code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"}, \code{"dotdash"}, \code{"longdash"} or \code{"twodash"}).

This parameter accepts a vector up to 2 elements in length. The first element is applied to the H0 boundary (or B boundary if applicable). The second element is applied to the H1 boundary (or A boundary if applicable).
}
  \item{lwd}{
the line width, a positive number, defaulting to 1. The interpretation is device-specific, and some devices do not implement line widths less than one.

This parameter accepts a vector up to 2 elements in length. The first element is applied to the H0 boundary (or B boundary if applicable). The second element is applied to the H1 boundary (or A boundary if applicable).
}
  \item{\dots}{
other graphical parameters, such as \code{cex}, \code{pch}, \code{family} and \code{font} (see \link{par}).}
}
\references{
Ghosh, B.K. and Sen, P.K. (1991). \emph{Handbook of Sequential Analysis}, Marcel Dekker, New York.
Wald, A. (1947). \emph{Sequential Analysis}, Dover, New York.
}
\author{
Stephane Mikael Bottine
}
\seealso{
\code{\link{par}}
\code{\link{plot.default}}
\code{\link{SPRT}}
}
\examples{
## Run the SPRT() function
test <- SPRT(distribution = "normal", type1 = 0.05, type2 = 0.20,
  h0 = 0, h1 = 1, values = rnorm(10))

## Plot k vs. H0 and H1 boundaries
plot(test)

## Plot the random variable's log-likelihood ratio vs. Wald's A and B constants
plot(test, log = "y")

## Draw a line through through dots
plot(test, log = "y", type = "b")

## Change the size and appearance of the dots
plot(test, log = "y", pch = 20, cex = 1.25)

## Use difference colours for the dots and boundaries
plot(test, log = "y", pch = 20, cex = 1.25, col = c(1,2,3))
}