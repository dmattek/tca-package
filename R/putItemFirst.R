#' Put a factor first
#'
#' Take a column with levels and put a specified factor first
#'
#' @param in.dt Input data table
#' @param in.col Name of the column to work on
#' @param in.item String with a factor to put as first
#'
#' @return Function changes state of the column provided by in.dt
#' @export
#' @import data.table
#'
#' @examples
putItemFirst = function(in.dt, in.col, in.item) {
  loc.v = unique(in.dt[[in.col]])
  loc.v.1st = as.vector(loc.v[loc.v %like% in.item])
  loc.v.other = as.vector(setdiff(loc.v, loc.v.1st))

  in.dt[, (in.col) := factor(get(in.col), levels = c(loc.v.1st, loc.v.other))]
}
