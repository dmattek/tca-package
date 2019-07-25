#' Keep a number of significant digits in a data.table
#'
#' @param dt Data in data.table format
#' @param digits Number of significant digits to keep
#'
#' @return Returns original data table with numeric fields trimmed to n significant digits.
#' @export
#' @import data.table
#'
#' @examples

signif_dt <- function(dt, digits) {
  loc.dt = copy(dt)

  loc.cols <- vapply(loc.dt, is.double, FUN.VALUE = logical(1))
  loc.cols = names(loc.cols[loc.cols])

  loc.dt[, (loc.cols) := signif(.SD, digits), .SDcols = loc.cols]

  return(loc.dt)
}


#' Keep a number of significant digits in a data.frame.
#'
#' @param df Data in data.frame format
#' @param digits Number of significant digits to keep
#'
#' @return Returns original data frame with numeric fields trimmed to n significant digits.
#' @export
#'
#' @examples
#'
signif_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- signif(df[,nums], digits = digits)

  (df)
}
