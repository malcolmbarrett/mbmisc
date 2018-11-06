#' These are real people, you dummy
#'
#' Run this plot every so often to remind yourself that you work with real
#' people, not data.
#'
#' @param .data A data frame
#' @param x Variable on the x axis
#' @param y Variable on the x axis
#' @param ... Other arguments passed to [ggplot2::aes()].
#' @param width Width of the subtitle
#'
#' @return a scatter plot
#' @export
these_are_real_people_dummy <- function(.data, x, y, ..., width = 70) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  ggplot2::ggplot(.data, ggplot2::aes(!!x, !!y, ...)) +
    geom_point2() +
    theme_malco() +
    ggplot2::labs(
      title = "These are real people, you dummy",
      subtitle = stringr::str_wrap("They aren't just data points. Changes in their health have actual impacts on their lives and the lives around them.", width = width)
    )
}
