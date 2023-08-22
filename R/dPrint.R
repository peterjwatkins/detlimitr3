#' Print output from 'lod' model
#'
#' @param x a 'lod' object
#' @param dp number of decimal points
#' @param ... optional parameters
#'
#' @export
#'
#' @examples
#' data(mtbe)
#'
#' m <- det.limit(mtbe$x, mtbe$y) # linear regression
#'
#' print(m, dp = 2) # two decimal points
print.lod <- function(x, dp = NULL, ...) {
  dp <- ifelse(is.null(dp), 1, dp)

  if (x$regression_type == "Linear") {
    cat("Linear regression \n")
    cat("----------------- \n")
    cat("Miller", round(dl_miller(x$x, x$y), dp), "\n")
    cat("Vogelsang-Hadrich", round(dl_vogelhad(x$x, x$y), dp), "\n")
    #    cat("chemCal", round(chemCal::lod((lm(
    #      d$y ~ d$x
    #    )))$x, dp), "\n")
    cat("Hubert-Vos", round(x$detection.limit[1], dp), "\n")
  } else if (x$regression_type == "Quadratic") {
    cat("Quadratic regression \n")
    cat("-------------------- \n")
    cat("Hubert-Vos", round(x$detection.limit[1], dp), "\n")
  } else if (x$regression_type == "Power") {
    cat("Power regression \n")
    cat("---------------- \n")
    cat("Hubert-Vos", round(x$detection.limit[1], dp), "\n")
  }
}
