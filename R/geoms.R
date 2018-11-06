#' Bigger Points
#'
#' `geom_points2()` is identical to [ggplot2::geom_point] except that it changes
#' some of the default aesthetics: points are bigger, use shape 21 (which has
#' fill and color aestherics) using a blue fill and white border color.
#' Additionally, the strokes around the point are slightly bigger.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @examples
geom_point2 <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
 ) {
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint2,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

GeomPoint2 <- ggplot2::ggproto(
  "GeomPoint2",
  ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 21,
    colour = "white",
    size = 3,
    fill = "#0172B1",
    alpha = NA,
    stroke = 0.75
  )
)
