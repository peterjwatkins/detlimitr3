#' Plot method for 'lod' objects
#' @description
#' Plot function to view calibration curve, based on linear,
#' quadratic or power regression.
#'
#' @param x a 'lod' object
#' @param residplot flag for regression residuals plot
#' @param ... optional parameters
#' @export
#'
#' @examples
#' data(mtbe)
#' m <- det.limit(mtbe$x, mtbe$y) # linear regression
#' plot(m)
#'
plot.lod <- function(x, residplot = NULL, ...) {
  residplot <- ifelse(is.null(residplot), FALSE, residplot)
  if (residplot ==  FALSE) {
    rt <- x$regression_type
    if (rt == "Linear")
      plotlinearDL(x$x, x$y, x$detection.limit[1])
    else if (rt == "Quadratic")
      plotqDL(x$x, x$y, x$detection.limit[1])
    else if (rt == "Power")
      plotpowerDL(x$x, x$y, x$detection.limit[1])
  }
  else if (residplot ==  TRUE || residplot == T)
    resid.plot(x$x, x$y, x$regression_type)
  else
    cat("Residual plot? Use either TRUE or T")
}
