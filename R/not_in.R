#' Not in
#'
#' `%not_in%` is the opposite of `%in%`. It is a binary operator, which
#' returns a logical vector indicating if there is not a match of the left hand
#' side in the right hand side vector.
#'
#' @param lhs vector or NULL: the values to be matched
#' @param rhs vector or NULL: the values to be matched against
#'
#' @return a logical vector
#' @export
#'
#' @examples
#'
#' 1 %not_in% 2:10 # TRUE
`%not_in%` <- function(lhs, rhs){
  match(lhs, rhs, nomatch = 0) == 0
}
