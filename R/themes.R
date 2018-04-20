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
palette_present <- function(index = NULL) {
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#E5E5E5", "#23373B")
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
theme_malco <- function(font_size = 14, font_family = "Roboto Condensed",
                        line_size = .5, color = "grey90") {
  half_line <- font_size / 2
  small_rel <- 0.857
  small_size <- small_rel * font_size

  ggplot2::theme_grey(base_size = font_size, base_family = font_family) %+replace%
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
        colour = "black",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = .9,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      axis.text = ggplot2::element_text(colour = "black", size = small_size),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = small_size / 4), vjust = 1),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = small_size / 4), hjust = 1),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = small_size / 2, b = small_size / 4)),
      axis.title.y = ggplot2::element_text(angle = 90,
                                                margin = ggplot2::margin(r = small_size / 2, l = small_size / 4)),
      axis.ticks = ggplot2::element_line(colour = color, size = line_size),
      axis.line = ggplot2::element_line(colour = "black",
                                                size = line_size,
                                                lineend = "square"),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = grid::unit(1.1 * font_size, "pt"),
      legend.spacing = grid::unit(font_size, "pt"),
      legend.box.spacing = grid::unit(font_size, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = ggplot2::element_text(size = ggplot2::rel(small_rel)),
      legend.justification = c("left", "center"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(colour = color,
                                                size = line_size),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(half_line, "pt"),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(small_rel),
        margin = ggplot2::margin(half_line, half_line, half_line, half_line)
      ),
      strip.background = ggplot2::element_rect(fill = "grey80", colour = "grey50"),
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = font_size + 2,
        hjust = 0,
        vjust = 1,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(small_rel),
        hjust = 0,
        vjust = 1,
        margin = ggplot2::margin(b = half_line * small_rel)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(small_rel),
        hjust = 1,
        vjust = 1,
        margin = ggplot2::margin(t = half_line * small_rel)
      ),
      plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
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
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      complete = TRUE
    )
}

#' Color text in R Markdown
#'
#' Color HTML or LaTeX text in R Markdown.
#'
#' @param x a string, the text to be colored
#' @param col the color
#'
#' @return a character vector
font_col <- function(x, col) {
  mkdwn_fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (mkdwn_fmt == "latex") {
    paste("\\textcolor{", color, "}{", x, "}", sep = "")
    glue::glue("\\textcolor{<<color>>}{<<x>>}",
               .open = "<<",
               .close = ">>")
  } else if (mkdwn_fmt == "html") {
    glue::glue("<span style = 'color:{color}'>{x}</span>")
  } else {
    x
  }
}
