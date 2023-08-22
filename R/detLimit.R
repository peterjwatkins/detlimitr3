#' Prepare a 'lod' object
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param reg_type regression type
#' @param ... optional parameters
#'
#' @usage det.limit(x, y, reg_type, ...)
#' @description A generic function for summarising the detection limits (DLs) according
#' to linear, quadratic or power regression. For linear regression, the DLs are
#' estimated using four approaches;i) Miller and Miller, ii) Vogelgesang and Hädrich and
#' iii) Hubert & Vos. The first two DLs are
#' directly estimated using internal functions, \emph{dl_miller} and \emph{dl_vogelhad}
#' while the third DL is estimated iteratively using \emph{dl_hubertvos}.
#' The default regression is assumed to be linear. However, the DLs can also be
#' estimated for either quadratic ("Quadratic") or power ("Power") regression.
#' For quadratic and power regression, the DLs are estimated using the intersection
#' of the upper prediction limit with the *x* = 0 line which then intersects
#' the lower prediction line. The associated *x* value is taken as the estimated detection limit.
#' See iv) for further details on this approach. The user is advised
#' though to verify the best regression approach by assessing the residual
#' plots.
#' @source {
#' i) J.C. Miller and J.N. Miller (1993), "Statistics for Analytical Chemistry",
#' 3rd ed., Prentice-Hall.
#' ii) J. Vogelgesang and J. Hädrich (1998), Accred. Qual. Assur., 3:242-255.
#' iii) A. Hubaux and G. Vos (1970), Anal. Chem., 42:849-855
#' & D.T. O'Neill, E.A. Rochette and P.J. Ramsay, (2002), Anal. Chem., 74:5907-5911
#' iv) D. Coleman and L. Vanatta (2009). American Laboratory, Statistics in Analytical Chemistry: Part 34 - Detection Limit Summary.
#' }
#' @export

#' @examples
#' data(mtbe)
#' m1 <- det.limit(mtbe$x, mtbe$y)  # linear regression as default
#' plot(m1, residplot = TRUE)          # review the residual plot
#' print(m1, dp = 2)                # print detection limits, with two decimal points
#' plot(m1)
#'
#' data(chloromethane)
#' m2 <- det.limit(chloromethane$Concentration, chloromethane$Response,
#'             "Quadratic")
#' plot(m2, residplot = TRUE)
det.limit <- function(x, y, reg_type = NULL, ...) {
  if (length(x) != length(y))
    cat("Lengths of x and y do not match\n")
  else
    if (!is.numeric(x) || !is.numeric(y))
      cat("x and/or y are not numeric\n")
  else
  {
    reg_type <- ifelse(is.null(reg_type), "l", reg_type)
    reg_type <- set_regression(reg_type)
    if (reg_type == "u")
      cat("Unknown regression type \n")
    else {
      if (reg_type == "Linear")
        dl <- dl_hubertvos(x, y)
      else if (reg_type == "Quadratic")
        dl <- dl_quad(x, y)
      else if (reg_type == "Power")
        dl <- dl_power(x, y)
      obj <-
        list(
          x = x,
          y = y,
          regression_type = reg_type,
          detection.limit = dl
        )
      # class can be set using class() or attr() function
      attr(obj, "class") <- "lod"
      return(obj)
    }
  }
}
