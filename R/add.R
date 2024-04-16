#' Sum of two vectors of numbers
#'
#' This function sums two vectors of numbers and optionally
#' allows for the negative of the sum to be returned.
#'
#' @param x a vector of numbers
#' @param y a vector of numbers
#' @param negative logical, whether to flip the sign of the sum
#'
#' @return the sum of the two vectors
#'
#' @examples
#'
#' add(1:5, 6:10)
#' add(1:5, 6:10, negative=TRUE)
#'
#' @importFrom gtools rdirichlet
#'
#' @export
add <- function(x,y,negative=FALSE) {
    z <- x + y
    if (negative) {
        z <- -1 * z
    }
    z
}
