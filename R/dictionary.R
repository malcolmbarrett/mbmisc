#' Search variable labels using regular expressions
#'
#' `dictionary()` searches variables that have a `label` attribute, like those
#' created with [Hmisc::label] or some functions in haven, for patterns.
#' Useful for searching large, labelled datasets.
#'
#' @param data a `data.frame`
#' @param pattern the pattern to search for
#'
#' @return a list with locations, names, and labels.
#' @export
dictionary <- function(data, pattern) {
  data_labels <- attr(data, "label")
  variable_index <- stringr::str_which(data_labels,
                                       stringr::regex(pattern, ignore_case = TRUE))
  variable_name <- names(data)[variable_index]
  list(index = variable_index,
       name = variable_name,
       labels = data_labels[variable_index])
}
