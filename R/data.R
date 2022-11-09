#' mRS data
#'
#' Randomly generated modified Rankin Scale data. Used to demonstrate `bmmisc::grotta_bar()`.
#'
#' @format A data frame with 400 generated observations of the following vars:
#' \describe{
#' \item{group}{Integer, study group}
#' \item{gender}{Factor, Male or Female}
#' \item{intv}{Factor, intervention status. Pre- or Post-intervention.}
#' \item{mRS}{Integer, modified Rankin Scale. A scale of 0-6 measuring degree of disability or dependence.
#' \itemize{
#'     \item 0: No symptoms
#'     \item 1: No significant disability
#'     \item 2: Slight disability
#'     \item 3: Moderate disability
#'     \item 4: Moderately severe disability
#'     \item 5: Severe disability
#'     \item 6: Dead
#'   }
#'   }
#'
#' }
#' @examples
#' xtabs(~mRS + intv, mRS)
#'
#' mRS
"mRS"
