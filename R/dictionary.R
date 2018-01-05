dictionary <- function(data, pattern) {
  data_labels <- Hmisc::label(data)
  variable_index <- stringr::str_which(data_labels, stringr::regex(pattern, ignore_case = TRUE))
  variable_name <- names(data)[variable_index]
  list(index = variable_index,
       name = variable_name,
       labels = data_labels[variable_index])
}
