#' Minimalistic themes
#'
#' Four minimalistic themes based on [ggplot2::theme_minimal()]. `theme_malco()`
#' provides a minimal grid theme. `theme_halco()` draws only horizontal lines
#' and `theme_valco()` only vertical lines. `theme_alco()` removes all axis and
#' grid ink. `theme_allgood()` is `theme_malco()` on a light gray background with
#' blue title text. These themes use the font IBM Plex Sans. You may want to use
#' `hrbrthemes::import_plex_sans()` and `extrafont::loadfonts()` to get it to
#' play nicely with PDFs
#'
#' @param font_size font size
#' @param font_family font family
#' @param line_size grid line size
#' @param text_color font color
#' @param color grid lines color
#'
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
    panel.spacing = grid::unit(font_size, "pt"),
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
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color = color) %+replace%
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
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color = color) %+replace%
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
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color) %+replace%
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
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color) %+replace%
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
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color = color, text_color = text_color) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = color),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), colour = "#0172B1", face = "bold"),
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_saulgood <- function(font_size = 14, font_family = "", line_size = .5, color = "grey92", text_color = "grey65") {
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color = color, text_color = text_color) %+replace%
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = color),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), colour = "#0172B1", face = "bold"),
      complete = TRUE
    )
}

#' @rdname theme_malco
#' @export
#' @importFrom ggplot2 %+replace%
theme_graphy <- function(font_size = 14, font_family = "", line_size = .5, color = "grey92", text_color = "grey65") {
  theme_malco(font_size = font_size, font_family = font_family, line_size = line_size, color = color, text_color = text_color) %+replace%
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      complete = TRUE
    )
}
