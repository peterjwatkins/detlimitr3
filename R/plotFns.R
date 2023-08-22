#-------------- Power -----------------------------------------------------------
plotpowerDL <- function(x, y, x_dl)  {
  dt <- calc_power(x, y)
  co <- upr_lwr_pwr_coefs(dt)

  y_dl <- co[1, 2]

  p <- baseDLplot(dt, x_dl, y_dl, c("Power"))
  return(p)
}

#--------------------Quadratic -------------------------------------------------
#' @importFrom stats lm predict
plotqDL <- function(x, y, x_dl) {
  model <- stats::lm(y ~ x + I(x ^ 2))
  pred_model <-
    stats::predict(model, newdata = as.data.frame(x), interval = "prediction")

  dt <-
    data.frame(cbind(
      x,
      y,
      y_fit = pred_model[, 1],
      y_lwr = pred_model[, 2],
      y_upr = pred_model[, 3]
    ))

  y_dl <- quad_beta(x, pred_model[, 3])[1, 1]

  p <- baseDLplot(dt, x_dl, y_dl, c("Quadratic"))
  return(p)
}
##' @importFrom stats lm predict coefficients
plotlinearDL <- function(x, y, x_dl) {
  d.lm <- stats::lm(y ~ x)
  pred_model <-
    stats::predict(d.lm, newdata = as.data.frame(x), interval = "prediction")

  y_fit <- pred_model[, 1]
  y_lwr <- pred_model[, 2]
  y_upr <- pred_model[, 3]

  dt <-
    data.frame(cbind(x,
                     y,
                     y_fit,
                     y_lwr,
                     y_upr))
  y_dl <-
    stats::coefficients(d.lm)[1] + stats::coefficients(d.lm)[2] * x_dl

  p <- baseDLplot(dt, x_dl, y_dl, c("Linear"))

  return(p)
}
#----------------------------------------------------------------------------
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme geom_segment xlab ylab annotate
baseDLplot <- function(d, x_dl, y_dl, tit) {
  x <- d$x
  y <- d$y
  y_fit <- d$y_fit
  y_lwr <- d$y_lwr
  y_upr <- d$y_upr
  p <-  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line((ggplot2::aes(x, y = y_fit, col = "blue"))) +
    ggplot2::geom_line((ggplot2::aes(x, y = y_lwr, col = "red"))) +
    ggplot2::geom_line((ggplot2::aes(x, y = y_upr, col = "red"))) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Concentration") +
    ggplot2::ylab("Response (AU)") +
    ggplot2::labs(title = tit) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        y = y_dl,
        xend = x_dl,
        yend = y_dl
      ),
      colour = "darkgreen",
      linetype = "dashed"
    )  +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = x_dl,
        y = y_dl,
        xend = x_dl,
        yend = 0
      ),
      colour = "darkgreen",
      linetype = "dashed"
    ) + ggplot2::annotate(
      "text",
      x = x_dl,
      y = 0,
      parse = TRUE,
      label = as.character(expression(italic("x")[HV]))
    ) +
    ggplot2::theme_bw() +
    theme(legend.position="none")
  return(p)
}
