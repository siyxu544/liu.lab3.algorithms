#' Find the greatest common divisor of two numbers
#'
#' This function implements the Euclidean algorithm to find the greatest
#' common divisor (GCD) of two numeric scalars.
#'
#' @param a A numeric scalar.
#' @param b A numeric scalar.
#' @return The greatest common divisor of a and b.
#' @references The description and pseudocode for the algorithm can be found
#'   at \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

euclidean <- function(a, b) {

  # Assert that the arguments are numeric scalars
  stopifnot(is.numeric(a), length(a) == 1,
            is.numeric(b), length(b) == 1)

  # Implement the Euclidean algorithm using a while loop
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }

  # Return the greatest common divisor
  return(a)
}
