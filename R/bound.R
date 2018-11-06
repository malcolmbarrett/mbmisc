#' Limit a numeric to be within a certain range.
#'
#' `bound()` limits a vector to a specific range. `sever()` trucates a vector to
#' a specific quantile.
#'
#' @param x a numeric vector
#' @param min minimum. Ignored if `NULL`.
#' @param max maximum. Ignored if `NULL`.
#' @param pctl percentile.
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#'
#' x <- rnorm(100, 8, 2)
#'
#' #  cap at 10
#' bound(x, max = 10)
#'
#' # truncate to 95th percentile
#' sever(x, pctl = 95)
#'
#' bound(1:100, 25, 75)
#'
#' @rdname limiters
bound <- function(x, min = NULL, max = NULL) {
  if (!is.null(min)) x <- ifelse(x < min, min, x)
  if (!is.null(max)) x <- ifelse(x > max, max, x)
  x
}

#' @rdname limiters
#' @export
#' @importFrom stats quantile
sever <- function(x, pctl = 99) {
  if (pctl > 1) pctl <- pctl / 100
  lower <- quantile(x, 1 - pctl)
  upper <- quantile(x, pctl)

  x <- ifelse(x < lower, lower, x)
  x <- ifelse(x > upper, upper, x)
  x
}
