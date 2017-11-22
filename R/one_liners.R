#' Print a loading matrix
#'
#' Prints a matrix with all entries below a given cut off removed. Useful
#' to print loading matrices in factor analysis.
#' @param loadings numerical matrix of variable loadings e.g.
#' from princomp()$loadings
#' @param cutoff numbers <= to cutoff will not be printed
#' @param digits number of significant digits to print
#'
#' @keywords pca fa factanal princomp
#' @export
#' @examples
#' set.seed(10)
#' lmprint(matrix(runif(9), 3, 3))
lmprint <- function(loadings, cutoff = 0.35, digits = 3) {
  print(ifelse(abs(loadings) > cutoff, loadings, NA),
        digits = digits, na.print = "")
}
#' is wholenumer
#'
#' Tests if numeric object is a whole number within a given tolerence.
#' Note: I copied this function from the web and I don't remember where from.
#'
#' @param x numeric object to be test
#' @param tol tolerence, default is sq root of machine epsilon
#'
#' @export
#' @keywords numeric
#' @examples
#' is.wholenumber(1.00000001, tol = 1.5e-08)
#' # TRUE
#' is.wholenumber(1.0000001, tol = 1.5e-08)
#' # FALSE
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol*2
}
