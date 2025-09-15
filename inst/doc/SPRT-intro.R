## ----message=FALSE------------------------------------------------------------
library(SPRT)

# Simulated binary outcomes (1 = success, 0 = failure)
x <- c(0,0,1,0,1,1,1,0,0,1,0,0)

# Run SPRT
res <- sprt(x, alpha = 0.05, beta = 0.1, p0 = 0.1, p1 = 0.3)

# Print results
res

# Plot sequential test path
sprt_plot(res)


# Observations from a Normal distribution
x1 <- c(52, 55, 58, 63, 66, 70, 74)

result1 <- sprt(
  x1,
  alpha = 0.05,
  beta = 0.1,
  p0 = 50,
  p1 = 65,
  dist = "normal",
  sigma = 10
)

result1
sprt_plot(result1)
# Yields from a fertilizer trial (kg/plot)
yield <- c(47, 50, 52, 49, 58, 61, 63, 54, 57)

fert_test <- sprt(
  yield,
  alpha = 0.05,
  beta = 0.1,
  p0 = 45,
  p1 = 55,
  dist = "normal",
  sigma = 8
)

fert_test
sprt_plot(fert_test)


