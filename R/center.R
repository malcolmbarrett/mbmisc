#' Center variables on their mean
#'
#' @param data a `data.frame`
#' @param ... variables to center. Can use `tidyselect` as in [dplyr::select()].
#'   If none are included, default is to center all numeric variables.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' center(mtcars)
#'
#' center(mtcars, hp)
#'
#' center(mtcars, starts_with("c"))
#'
#' @importFrom rlang !!!
center <- function(data, ..., na.rm = TRUE) {
  cen_vars <- rlang::enquos(...)
  browser()
  if (rlang::is_empty(cen_vars)) {
    data <- data %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(. - mean(., na.rm = na.rm)))
  } else {
    data <- data %>%
      dplyr::mutate_at(dplyr::vars(!!!cen_vars),
                       dplyr::funs(. - mean(., na.rm = na.rm)))
  }

  data
}
