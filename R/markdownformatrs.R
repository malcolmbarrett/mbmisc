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
col_code <- function(x, col) {
  font_col(x, col = col, code = TRUE)
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

#' @export
#' @name color
white <- function(x, code = FALSE) {
  font_col(x, "#EDEEEF", code = code)
}
