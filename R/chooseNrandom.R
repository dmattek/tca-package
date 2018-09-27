#' Randomly choose rows by group
#'
#' Takes N random rows from a data table. If grouping column provided, sampling is performed by group.
#'
#' @param in.dt Input data table
#' @param in.n Number of rows to sample
#' @param in.col.id Name of the column with unique row id's
#' @param in.col.group Name of the grouping column
#'
#' @return Data table with sampled rows
#' @export
#' @import data.table
#'
#' @examples
chooseNrandom = function(in.dt, in.n, in.col.id, in.col.group) {
  loc.v.sel = in.dt[, .SD[sample(.N, min(in.n, .N))], by = in.col.group][[in.col.id]]
  loc.dt = in.dt[get(in.col.id) %in% loc.v.sel]

  return(loc.dt)
}
