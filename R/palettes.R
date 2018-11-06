#' Custom color palettes
#'
#' These palettes are losely based on colorblind-friendly palettes from the
#' `colorblindr` package and colors related to those in the `xaringan` package.
#'
#' @param index a numeric vector, the colors to include
#'
#' @return a character vector
#' @export
#' @rdname palettes
palette_malco <- function(index = NULL) {
  pal <- c(
    "#56B4EA",
    "#CC78A8",
    "#E7A000",
    "#0172B1",
    "#4E834B",
    "#474747",
    "#EDEEF0"
  )
  if (!is.null(index)) pal[index] else pal
}

#' @export
#' @rdname palettes
palette_highlight <- function(index = NULL) {
  pal <- c("#D55E00", "#0072B2", "#CCCCCC", "#474747")
  if (!is.null(index)) pal[index] else pal
}
