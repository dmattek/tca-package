

#' Calculate 95% CI from the mean and SD (normal assumption)
#'
#' From: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)
#'
#' @param in.dt Input data table
#' @param in.col.mn Name of the column containing the mean
#' @param in.col.sd Name of the column containing SD
#' @param in.col.n Name of the column containing sample size
#' @param in.clevel Confidence level, default 95%
#'
#' @return Data table with additional columns with lower and upper CI bounds
#' @export
#' @import data.table
#'
#' @examples

calcCI = function(in.dt, in.col.mn, in.col.sd, in.col.n, in.clevel = 0.95) {

  loc.dt = copy(in.dt)

  # calculate SE
  loc.col.se = paste0(in.col.mn, ".se")
  loc.dt[, (loc.col.se) := get(in.col.sd) / sqrt(get(in.col.n))]


  # claculate CI
  loc.dt[, ci.tmp := get(loc.col.se) * qt(in.clevel / 2 + .5, get(in.col.n) - 1)]

  loc.col.cilo = paste0(in.col.mn, '.cilo')
  loc.col.cihi = paste0(in.col.mn, '.cihi')
  loc.dt[, (loc.col.cilo) := get(in.col.mn) - ci.tmp]
  loc.dt[, (loc.col.cihi) := get(in.col.mn) + ci.tmp]
  loc.dt[, ci.tmp := NULL]

  return(loc.dt)
}
