#' is.wholenumer()
#'
#' Tests if numeric object is a whole number within a given tolerence.
#' Note: I copied this function from the web and I don't remember where from.
#'
#' @param x numeric object to be test
#' @param tol tolerence, default is sq root of machine epsilon
#'
#' @examples
#' is.wholenumber(1.00000001, tol = 1.5e-08)
#' # TRUE
#' is.wholenumber(1.0000001, tol = 1.5e-08)
#' # FALSE

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol*2
