#   Revised scripts for linear, quadratic and power regression model types

#' @importFrom ggplot2 ggplot aes geom_point xlab ylab labs
base.resid.plot <- function(x, y, tit) {
  d <- as.data.frame(cbind(x, y))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Fitted") +
    ggplot2::ylab("Residuals") +
    ggplot2::labs(title = tit) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none")
}
#------------------------------------------------------------------------------
#' @importFrom stats lm fitted resid nls
resid.plot <- function(x, y, regression_type) {
  switch(
    regression_type,
    "Linear" = {
      model = stats::lm(y ~ x)
      base.resid.plot(stats::fitted(model),
                      stats::resid(model),
                      c("Linear"))
    },
    "Quadratic" = {
      model = stats::lm(y ~ x + I(x ^ 2))
      base.resid.plot(stats::fitted(model),
                      stats::resid(model),
                      c("Quadratic"))
    },
    "Power" = {
      par <- ss.pwr(x, y)
      model <-
        minpack.lm::nlsLM(y ~ C + A * x ^ b,
                          start = list(C = par[1], A = par[2], b = par[3]))
      base.resid.plot(stats::fitted(model),
                      stats::resid(model),
                      c("Power"))
    }
  )
}
