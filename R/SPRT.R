#' Sequential Probability Ratio Test (SPRT)
#'
#' Performs the SPRT for Bernoulli, Normal, or Poisson data.
#'
#' @param x Vector of observed values.
#' @param alpha Type I error rate.
#' @param beta Type II error rate.
#' @param p0 Null hypothesis parameter (probability or mean).
#' @param p1 Alternative hypothesis parameter (probability or mean).
#' @param dist Distribution: "bernoulli", "normal", or "poisson".
#' @param sigma Standard deviation (for normal distribution only).
#'
#' @return A list with elements:
#'   \item{decision}{"Accept H0", "Reject H0", or "Continue sampling"}
#'   \item{n_decision}{Step at which decision was made (NA if continue)}
#'   \item{logL}{Cumulative log-likelihood ratios for each step}
#'   \item{A}{Upper threshold (log scale)}
#'   \item{B}{Lower threshold (log scale)}
#'
#' @examples
#' x <- c(0,0,1,0,1,1,1,0,0,1,0,0)
#' res <- sprt(x, alpha = 0.05, beta = 0.1, p0 = 0.1, p1 = 0.3)
#' print(res)
#' x1 <- c(52, 55, 58, 63, 66, 70, 74)
#' result1 <-sprt(x1, alpha = 0.05, beta = 0.1, p0 = 50, p1 = 65, dist = "normal", sigma = 10)
#' result1
#' @export
sprt <- function(x, alpha = 0.05, beta = 0.05,
                 p0, p1, dist = c("bernoulli", "poisson", "normal"),
                 sigma = 1) {

  dist <- match.arg(dist)

  # Thresholds
  A <- log((1 - beta) / alpha)
  B <- log(beta / (1 - alpha))

  logL <- numeric(length(x))
  s <- 0
  n_decision <- NA

  for (i in seq_along(x)) {
    xi <- x[i]

    if (dist == "bernoulli") {
      num <- ifelse(xi == 1, p1, 1 - p1)
      den <- ifelse(xi == 1, p0, 1 - p0)
      s <- s + log(num / den)
    } else if (dist == "poisson") {
      s <- s + log(stats::dpois(xi, lambda = p1) / stats::dpois(xi, lambda = p0))
    } else if (dist == "normal") {
      s <- s + log(stats::dnorm(xi, mean = p1, sd = sigma) /
                   stats::dnorm(xi, mean = p0, sd = sigma))
    }

    logL[i] <- s

    if (s >= A) {
      decision <- "Reject H0"
      n_decision <- i
      return(list(decision = decision, n_decision = n_decision, logL = logL[1:i], A = A, B = B))
    }

    if (s <= B) {
      decision <- "Accept H0"
      n_decision <- i
      return(list(decision = decision, n_decision = n_decision, logL = logL[1:i], A = A, B = B))
    }
  }

  decision <- "Continue sampling"
  return(list(decision = decision, n_decision = n_decision, logL = logL, A = A, B = B))
}
