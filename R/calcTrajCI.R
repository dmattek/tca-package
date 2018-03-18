#' Calculate the mean and CI around time series
#'
#' @param in.dt Data table in long format
#' @param in.col.meas Name of the column with the measurement
#' @param in.col.by Column names for grouping (default NULL - no grouping). Typically, you want to use at least a column with time.
#' @param in.type Choice of normal approximation or boot-strapping
#' @param ... Other params passed to smean.cl.normal and smean.cl.boot; these include \code{conf.int} for the confidence level, \code{B} for the number of boot-strapping iterations.
#'
#' @return
#' @export
#' @import data.table
#' @import Hmisc
#'
#' @examples
#'
#' require(tca)
#'
#' # generate synthetic time series; 100 time points long, with 10 randomly placed NAs
#' dt.tmp = genTraj(100, 10, 6, 3, in.addna = 10)
#'
#' # calculate single stats from all time points
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor')
#'
#' # calculate the mean and CI along the time course
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor', 'Metadata_RealTime')
calcTrajCI = function(in.dt, in.col.meas, in.col.by = NULL, in.type = c('normal', 'boot'), ...) {
  in.type = match.arg(in.type)

  if (in.type %like% 'normal')
    loc.dt = in.dt[, as.list(smean.cl.normal(get(in.col.meas), ...)), by = in.col.by] else
      loc.dt = in.dt[, as.list(smean.cl.boot(get(in.col.meas), ...)), by = in.col.by]

  return(loc.dt)
}
