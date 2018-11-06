#' Custom color palettes
#'
#' These palettes are based on colorblind-friendly palettes from the
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

#' Minimalistic themes
#'
#' Four minimalistic themes based on [cowplot::theme_minimal_grid()].
#' `theme_malco()` provides a minimal grid theme. `theme_halco()` draws only
#' horizontal lines and `theme_valco()` only vertical lines. `theme_alco()`
#' removes all axis and grid ink.
#'
#' @param font_size font size.
#' @param font_family font family
#' @param line_size grid line size
#' @param color grid lines color
#' @export
#' @importFrom ggplot2 %+replace%
#' @rdname theme_malco
theme_malco <- function(font_size = 14, font_family = "IBM Plex Sans",
                        line_size = .5, color = "grey90", text_color = "grey70") {
  half_line <- font_size / 2

  ggplot2::theme_minimal(base_size = font_size, base_family = font_family) %+replace%
    ggplot2::theme(
      rect = ggplot2::element_rect(
        fill = "transparent",
        colour = NA,
        color = NA,
        size = 0,
        linetype = 0
      ),
      text = ggplot2::element_text(
        family = font_family,
        face = "plain",
        colour = text_color,
        size = font_size,
        angle = 0,
        debug = FALSE,
        margin = ggplot2::margin(.5, .5, .5, .5),
        hjust = 0,
        vjust = .5,
        lineheight = .9
      ),
    axis.text = ggplot2::element_text(size = ggplot2::rel(.9)),
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), colour = "#474747", face = "bold"),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.2)),
    panel.grid = ggplot2::element_line(colour = color),
    panel.spacing = unit(font_size, "pt"),
    strip.text = ggplot2::element_text(hjust = 0),
    axis.title.x = ggplot2::element_text(hjust = 0),
    axis.title.y = ggplot2::element_text(angle = 90, hjust = 1),
    axis.text.x = ggplot2::element_text(hjust = .5),
    axis.text.y = ggplot2::element_text(hjust = .5),
    legend.position = "bottom",
    complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_valco <- function(font_size = 14, font_family = "", line_size = .5, color = "grey90") {
  theme_malco(font_size = font_size, font_family = font_family, line_size, color) %+replace%
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = color, size = line_size),
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_halco <- function(font_size = 14, font_family = "", line_size = .5, color = "grey90") {
  theme_malco(font_size = font_size, font_family = font_family, line_size, color) %+replace%
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = color, size = line_size),
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_alco <- function(font_size = 14, font_family = "", line_size = .5, color = "grey90") {
  theme_malco(font_size = font_size, font_family = font_family, line_size, color) %+replace%
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_barr <- function(font_size = 14, font_family = "", line_size = .5, color = "grey90") {
  theme_malco(font_size = font_size, font_family = font_family, line_size, color) %+replace%
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_allgood <- function(font_size = 14, font_family = "", line_size = .5, color = "grey92", text_color = "grey65") {
  theme_malco(font_size = font_size, font_family = font_family, line_size, color = color, text_color = text_color) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = color),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), colour = "#0172B1", face = "bold"),
      complete = TRUE
    )
}

#' mbmisc ggplot scales
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
#' ggplot(mtcars, aes(mpg, hp, col = am)) +
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

#' Color text in R Markdown
#'
#' Color HTML or LaTeX text in R Markdown.
#'
#' @param x a string, the text to be colored
#' @param col the color
#' @param code logical. Is this code?
#'
#' @return a character vector
#' @export
#' @name color
font_col <- function(x, col, code = FALSE) {
  mkdwn_fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (mkdwn_fmt == "latex") {
    code_srt <- ifelse(code, "\texttt{", "")
    code_end <- ifelse(code, "}", "")
    glue::glue("<<code_srt>>\\textcolor{<<col>>}{<<x>>}<<code_end>>",
               .open = "<<",
               .close = ">>")
  } else if (mkdwn_fmt == "html") {
    code_srt <- ifelse(code, "<code>", "")
    code_end <- ifelse(code, "</code>", "")
    glue::glue("{code_srt}<span style = 'color:{col}'>{x}</span>{code_end}")
  } else {
    x
  }
}

#' @export
#' @name color
blue <- function(x, code = FALSE) {
  font_col(x, "#56B4E9", code = code)
}

#' @export
#' @name color
dark_blue <- function(x, code = FALSE) {
  font_col(x, "#0162B2", code = code)
}

#' @export
#' @name color
orange <- function(x, code = FALSE) {
  font_col(x, "#E69F00", code = code)
}

#' @export
#' @name color
dark_orange <- function(x, code = FALSE) {
  font_col(x, "#FF751A", code = code)
}

#' @export
#' @name color
dark_gray <- function(x, code = FALSE) {
  font_col(x, "#6C7B7F", code = code)
}

#' @export
#' @name color
dark_grey <- dark_gray

#' @export
#' @name color
light_gray <- function(x, code = FALSE) {
  font_col(x, "#E5E5E5", code = code)
}

#' @export
#' @name color
light_grey <- light_gray
