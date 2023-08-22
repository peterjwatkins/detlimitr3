#-----------------------------------------------------------------
least_sq_est <- function(x, y) {
  # Used internally for calculating linear least square regression coefficients
  d.lm <- lm(y~x)
  return(c(m = coef(d.lm)[2], c = coef(d.lm)[1]))
}
s_y <- function(x, y) {
  # Used internally to calculate residual standard deviation
  #
  n <- length(x)
  ls <- least_sq_est(x, y)
  return(sqrt(sum((y - (
    ls[1] * x + ls[2]   # ls[1]*x + ls[2] = fitted y
  )) ^ 2) / (n - 2)))
}

#------------------------ Calculation ------------------------------------------
dl_miller <- function(x, y) {
  # Used internally to estimate the detection limit according to
  # Miller & Miller
  n <- length(x)
  ls <- least_sq_est(x, y)
  dl_calc <- (3 * s_y(x, y)) / ls[1]
  MMdl <- as.numeric(dl_calc)
  return(MMdl)
}
# Extra code if needed
#    dl_blank <- (3 * s_y(x, y) + ls[2]) / ls[1]
#    MMdl <- as.numeric(c(dl_c = dl_calc,
#              dl_b = dl_blank))

#' @importFrom stats qt
dl_vogelhad <- function(x, y) {
  # Used internally to estimate the detection limit according to
  # Vogelgesang & Hädrich
  n <- length(x)
  ls <- least_sq_est(x, y)
  y_crit <-
    ls[2] + s_y(x, y) * qt(0.95, n - 2) *
    sqrt(1 + 1 / n + mean(x) ^ 2 / sum((x - mean(x)) ^ 2))
  x_crit <- (y_crit - ls[2]) / ls[1]
  VHdl <- as.numeric(x_crit)
  return(VHdl)
}
#   Extra code if needed
#   x_id <- 2 * x_crit
#   VHdl <- as.numeric(c(xc = x_crit, xd = x_id))
#' @importFrom stats qt
dl_hubertvos <- function(x, y, alpha = NULL, beta = NULL) {
  # Used internally to estimate the detection limit acccording to
  # Hubert & Vos detection limit using iterative calculation
  alpha <- ifelse(is.null(alpha), 0.01, alpha)
  beta <- ifelse(is.null(beta), 0.05, beta)
  n <- length(x)
  x.mean <- mean(x)
  HVdl <- dl_vogelhad(x, y)
  repeat {
    # Update dl
    new.dl <-
      s_y(x, y) / least_sq_est(x, y)[1] * (
        stats::qt(1 - alpha, n - 2) * sqrt(1 + 1 / n + x.mean ^ 2 / sum((x - x.mean) ^
                                                                          2)) +
          stats::qt(1 - beta, n - 2) * sqrt(1 + 1 /
                                              n + (HVdl - x.mean) ^ 2 / sum((x - x.mean) ^ 2))
      )
    # Compute relative error as a 2-norm.
    conv <- sum((new.dl - HVdl) ^ 2 / HVdl ^ 2)
    # Exit test with return() statement
    if (conv < 1e-10)
      return(as.numeric(HVdl))
    # Save interation result
    HVdl <- new.dl
  }
}
