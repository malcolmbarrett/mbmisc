round_with_zeros <- function(.x, digits = 2) {
  format(round(.x, digits = digits), trim = TRUE, nsmall = digits)
}

clean_pval <- function(x, less_than = .001, digits = 3, add_stars = FALSE,
                       stars = list(pval = c(.1, .05, .01, .001),
                                    symbol = c(".", "*", "**", "***"))) {
  # todo: make these more dynamic above so user can control, add_stars option
  ifelse(x < less_than, paste0("<", less_than), round_with_zeros(x, digits))
}

make_formula <- function(y, x) {
  as.formula(paste(y, "~", paste(x, collapse = " + ")))
}

ci95 <- "95% CI "
ci90 <- "90% CI "
ci99 <- "99% CI "
minmax <- paste("Min.-Max.")
iqr <- "IQR "
none <- ""
parenthesis <- c("(", ")")
brackets <- c("[", "]")
dash <- "-"
comma <- ", "

est_ci <- function(est, lower = NULL, upper = NULL, var = NULL, bound = parenthesis, divider = comma, descriptor = ci95, digits = 2) {
  if (length(bound) > 1 && bound == "") bound <- c("", "")

  if (is.data.frame(est)) {

    lower <- est[est$term == var, "conf.low"]
    upper <- est[est$term == var, "conf.high"]

    if (is.null(var)) {
      est <- paste0(est[["term"]], ": ",
                    round_with_zeros(est[["estimate"]], digits = digits))
    } else {
      if (!is.character(var)) stop("`var` must be of type character")
      est <- est[est$term == var, "estimate"]
      est <- round_with_zeros(est, digits = digits)
      }
  } else {
    est <- round_with_zeros(est, digits = digits)
  }

  paste0(est, " ",
         bound[1],
         descriptor,
         round_with_zeros(lower, digits = digits),
         divider,
         round_with_zeros(upper, digits = digits),
         bound[2])
}
