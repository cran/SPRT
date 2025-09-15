#' Plot SPRT results
#'
#' @param res A list returned by `sprt()`.
#' @return A ggplot object showing the SPRT path with thresholds and decision point.
#' @importFrom ggplot2 ggplot aes geom_line geom_hline theme_minimal labs annotate
#' @importFrom rlang .data
#'
#' @examples
#' x <- c(0,0,1,0,1,1,1,0,0,1,0,0)
#' res <- sprt(x, alpha = 0.05, beta = 0.1, p0 = 0.1, p1 = 0.3)
#' print(res)
#' sprt_plot(res)
#'
#' x1 <- c(52, 55, 58, 63, 66, 70, 74)
#' result1 <- sprt(x1, alpha = 0.05, beta = 0.1, p0 = 50, p1 = 65, dist = "normal", sigma = 10)
#' result1
#' sprt_plot(result1)
#' @export
sprt_plot <- function(res) {

  df <- data.frame(
    Step = seq_along(res$logL),
    Value = res$logL
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Step, y = .data$Value)) +
    ggplot2::geom_line(color = "blue", linewidth = 1) +
    ggplot2::geom_hline(yintercept = res$A, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = res$B, linetype = "dashed", color = "green") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Sequential Probability Ratio Test (SPRT)",
      x = "Step",
      y = "Cumulative Log-Likelihood Ratio"
    )

  # Add decision point if available
  if (!is.na(res$n_decision)) {
    p <- p +
      ggplot2::annotate("point", x = res$n_decision, y = res$logL[res$n_decision],
                        color = "red", size = 3) +
      ggplot2::annotate("text", x = res$n_decision, y = res$logL[res$n_decision],
                        label = res$decision, vjust = -1, color = "red")
  }

  return(p)
}
