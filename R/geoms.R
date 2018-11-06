#' Title
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return
#' @export
#'
#' @examples
geom_point2 <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
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
    stroke = 0.5
  )
)
