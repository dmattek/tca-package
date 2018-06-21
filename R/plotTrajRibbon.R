#' Plot time series with CI ribbon
#'
#' A ribbon with confidence interval for the mean is plotted around the group mean.
#' Optionally an additional curve is plotted, e.g. a stimulation time course.
#'
#' @param dt.arg Data table in long format with mean time series and upper and lower bound for the CI
#' @param x.arg String with column name of x-axis data (default: Metadata_RealTime)
#' @param y.arg String with column name of y-axis data (default: Mean)
#' @param group.arg String with column name of the grouping variable for trajectories (default: Metadata_Well).
#' @param col.arg Vector with string elements with colour palette for time series curves (default: NULL).
#' @param dt.stim.arg Data table with stimulation time series (default: NULL).
#' @param stim.x.arg String with column name of x-axis of dt.stim.arg.
#' @param stim.y.arg String with column name of y-axis of dt.stim.arg.
#' @param xlab.arg String with x-axis label
#' @param ylab.arg String with y-axis label
#' @param plotlab.arg String with plot title
#'
#' @return ggplot2 plot object
#' @export plotTrajRibbon
#' @import ggplot2
#'
#' @examples
#'
#' require(tca)
#'
#' # generate synthetic time series
#' dt = genTraj()
#'
#' # clculate 90% CI
#' dt.aggr = calcTrajCI(in.dt = dt, in.col.meas = 'objNuc_Intensity_MeanIntensity_imErkCor', in.col.by = 'Metadata_Site', in.type = 'normal', conf.int = 0.9)
#'
#' # plot each group in a separate facet; add population mean
#' plotTrajRibbon(dt.arg = dt.aggr, x.arg = 'Metadata_RealTime', y.arg = 'Mean')

plotTrajRibbon = function(dt.arg,
                          x.arg,
                          y.arg,
                          group.arg = NULL,
                          col.arg = NULL,
                          dt.stim.arg = NULL,
                          stim.x.arg,
                          stim.y.arg,
                          ribbon.lohi.arg = c('Lower', 'Upper'),
                          ribbon.fill.arg = 'grey50',
                          ribbon.alpha.arg = 0.5,
                          xlab.arg = NULL,
                          ylab.arg = NULL,
                          plotlab.arg = NULL) {

  p.tmp = ggplot(dt.arg, aes_string(x = x.arg, group = group.arg)) +
    geom_ribbon(aes_string(ymin = ribbon.lohi.arg[1], ymax = ribbon.lohi.arg[2]),
                fill = ribbon.fill.arg,
                alpha = ribbon.alpha.arg) +
    geom_line(aes_string(y = y.arg, colour = group.arg))


  if (!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_line(
      data = dt.stim.arg,
      aes_string(x = stim.x.arg, y = stim.y.arg),
      colour = 'blue',
      size = 1,
      group = 1
    )
  }

  if (is.null(col.arg)) {
	  p.tmp = p.tmp +
      scale_color_discrete(name = '')
  } else {
	  p.tmp = p.tmp +
	  	scale_colour_manual(values = col.arg, name = '')
  }

  if (!is.null(plotlab.arg))
    p.tmp = p.tmp + ggtitle(plotlab.arg)

  p.tmp = p.tmp +
    xlab(xlab.arg) +
    ylab(ylab.arg)

  return(p.tmp)
}
