#' mbmisc ggplot scales
#'
#' These functions use the colors in `palette_malco()` to work easily with
#' ggplot2 and ggraph.
#'
#' @param aesthetics What type of aesthetic?
#' @param order What's the order of the palette?
#' @param alpha A numeric vector
#' @param ... Passed to [ggplot2::discrete_scale]
#'
#' @return a ggplot scale
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(mpg, hp, col = factor(am))) +
#'   geom_point() +
#'   scale_color_malco()
#'
scale_malco <- function(aesthetics, order = 1:7, alpha = NULL, ...) {
  values <- palette_malco(order)
  n <- length(values)

  if (!is.null(alpha)) values <- scales::alpha(values, alpha)
  pal <- function(n) {
      if (n > length(values)) {
          warning("Insufficient values in manual scale. ",
              n, " needed but only ", length(values), " provided.",
              call. = FALSE)
      }
      values
  }
  ggplot2::discrete_scale(aesthetics, "manual", pal, ...)
}

#' @export
#' @name scale_malco
scale_colour_malco <- function(order = 1:7, alpha = NULL, ...) {
  scale_malco(aesthetics = "colour", order = order, alpha = alpha, ...)
}

#' @export
#' @name scale_malco
scale_color_malco <- scale_colour_malco

#' @export
#' @name scale_malco
scale_fill_malco <- function(order = 1:7, alpha = NULL, ...) {
  scale_malco(aesthetics = "fill", order = order, alpha = alpha, ...)
}

#' @export
#' @name scale_malco
scale_edge_colour_malco <- function(order = 1:7, alpha = NULL, ...) {
  scale_malco(aesthetics = "edge_colour", order = order, alpha = alpha, ...)
}

#' @export
#' @name scale_malco
scale_edge_color_malco <- scale_edge_colour_malco

#' @export
#' @name scale_malco
scale_edge_fill_malco <- function(order = 1:7, alpha = NULL, ...) {
  scale_malco(aesthetics = "edge_fill", order = order, alpha = alpha, ...)
}
