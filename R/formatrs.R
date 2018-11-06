#' Round without dropping zeroes
#'
#' This function rounds a number but forces any dangling zeroes to remain.
#'
#' @param .x a numeric vector
#' @param digits number of digits to round to
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' round_with_zeros(1.201)
#' round_with_zeros(rnorm(10))
round_with_zeros <- function(.x, digits = 2) {
  format(round(.x, digits = digits), trim = TRUE, nsmall = digits)
}

#' Clean p-value
#'
#' This function cleans a p-value by rounding to a certain digit and changing
#' any values less than a certain amount to, e.g., "<.001".
#'
#' @param x a vector of p-values
#' @param less_than the lower bound for printing complete numbers
#' @param digits the number of digits to round to
#' @param add_stars logical. Add stars to the p-value?
#' @param stars a list of p-values cut-offs and matching symbols
#'
#' @return a character vector
#' @export
#'
#' @examples
#' clean_pval(1e-10)
clean_pval <- function(x, less_than = .001, digits = 3, add_stars = FALSE,
                       stars = list(pval = c(.1, .05, .01, .001),
                                    symbol = c(".", "*", "**", "***"))) {
  # todo: make these more dynamic above so user can control, add_stars option
  ifelse(x < less_than, paste0("<", less_than), round_with_zeros(x, digits))
}

#' Make a formula
#'
#' Quickly make a formula out of character vectors
#'
#' @param y the outcome variable
#' @param x the exposure variables (a character vector)
#'
#' @return a formula
#' @export
#'
#' @examples
#'
#' make_formula("y", paste0("x", 1:10))
#' @importFrom stats as.formula
make_formula <- function(y, x) {
  as.formula(paste(y, "~", paste(x, collapse = " + ")), env = parent.frame())
}

#' Paste an estimate and CI together
#'
#' Quickly concatenate an estimate with its confidence intervals. Works with
#' vectors or output from [broom::tidy()].
#'
#' @param est either `a data.frame` or a vector of estimates
#' @param lower the lower CI. if `NULL`, will search for "conf.low"
#' @param upper the upper CI. if `NULL`, will search for "conf.high"
#' @param var estimate name
#' @param bound bounding symbols around the CI. Default is parentheses.
#' @param divider a divider between the upper and lower CI. Default is a comma.
#' @param descriptor a descriptor of the CI. Default is "95\\% CI".
#' @param include_name logical. Should the name of the term be included?
#' @param digits the number of digits to round to
#' @param lvl the confidence interval probability
#'
#' @return a `data.frame` or vector.
#' @export
#'
#' @examples
#'
#' library(broom)
#' library(dplyr)
#'
#' x <- tidy(lm(mpg ~ hp, data = mtcars), conf.int = TRUE)
#' x %>%
#'   mutate(eci = est_ci(estimate, conf.low, conf.high))
#'
#' est_ci(x)
#'
est_ci <- function(est, lower = NULL, upper = NULL, var = NULL,
                   include_name = FALSE, bound = parenthesis(),
                   divider = comma(), descriptor = ci(95), digits = 2) {
  if (length(bound) > 1 && bound == "") bound <- c("", "")

  if (is.data.frame(est)) {

    if (is.null(var)) {
      lower <- est[["conf.low"]]
      upper <- est[["conf.high"]]
      estmt <- round_with_zeros(est[["estimate"]], digits = digits)
      if (include_name) estmt <- paste0(est[["term"]], ": ", estmt)
    } else {
      if (!is.character(var)) stop("`var` must be of type character")
      lower <- est[est$term == var, "conf.low"]
      upper <- est[est$term == var, "conf.high"]
      estmt <- est[est$term == var, "estimate"]
      estmt <- round_with_zeros(estmt, digits = digits)
      }
  } else {
    estmt <- round_with_zeros(est, digits = digits)
  }

  paste0(estmt, " ",
         bound[1],
         descriptor,
         round_with_zeros(lower, digits = digits),
         divider,
         round_with_zeros(upper, digits = digits),
         bound[2])
}

#' @export
#' @rdname est_ci
ci <- function(lvl = 95) paste0(lvl, "% CI ")

#' @export
#' @rdname est_ci
minmax <- function() paste("Min.-Max.")

#' @export
#' @rdname est_ci
iqr <- function() "IQR "

#' @export
#' @rdname est_ci
none <- function() ""

#' @export
#' @rdname est_ci
parenthesis <- function() c("(", ")")

#' @export
#' @rdname est_ci
brackets <- function() c("[", "]")

#' @export
#' @rdname est_ci
dash <- function() "-"

#' @export
#' @rdname est_ci
comma <- function() ", "
