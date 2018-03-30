#' Plot individual time series
#'
#' @param dt.arg Data table in long format
#' @param x.arg String with column name of x-axis data
#' @param y.arg String with column name of y-axis data
#' @param group.arg String with column name of the grouping variable for trajectories. This is usually a trackID.
#' @param facet.arg String with oclumn name of the variable used for plot facets (optional)
#' @param summary.arg Optional summary stats to plot on top of individual time series. Choose from \code{c('none', 'mean', 'meanCInorm', 'meanCIboot')},
#' to plot no extra stats, the mean, the mean with 95\% CI calculated from the normal distribution approximation, the mean with 95\% CI calculated via bootstrapping, respectively.
#' @param facet.ncol.arg Number of facetting columns
#' @param line.col.arg String with variable name to use for time series colouring (default NULL)
#' @param xlab.arg String with x-axis label
#' @param ylab.arg String with y-axis label
#' @param plotlab.arg String with plot title
#' @param dt.stim.arg Data table with stimulation curve
#' @param stim.x.arg String with column name of x-axis stimulation data
#' @param stim.y.arg String with column name of y-axis stimulation data
#' @param maxrt.arg Max value of x-axis label
#' @param xaxisbreaks.arg Value of x-axis interval
#' @param xlim.arg Limit of x-axis
#' @param ylim.arg Limit of y-axis
#'
#' @return ggplot2 plot object
#' @export
#' @import ggplot2
#'
#' @examples
#'
#' require(tca)
#'
#' # generate synthetic time series
#' dt = genTraj()
#'
#' # plot each group in a separate facet; add population mean
#' plotTraj(dt.arg = dt, x.arg = 'Metadata_RealTime', y.arg = 'objNuc_Intensity_MeanIntensity_imErkCor', group.arg = 'TrackLabel', facet.arg = 'Metadata_Site', summary.arg = 'mean')

plotTraj = function(dt.arg,
                        x.arg,
                        y.arg,
                        group.arg,
                        facet.arg = NULL,
                        summary.arg = c('none', 'mean', 'meanCInorm', 'meanCIboot'),
                        facet.ncol.arg = 2,
                        line.col.arg = NULL,
                        xlab.arg = "Time",
                        ylab.arg = "Fl. int.",
                        plotlab.arg = "",
                        dt.stim.arg = NULL,
                        stim.x.arg,
                        stim.y.arg,
                        maxrt.arg = 60,
                        xaxisbreaks.arg = 10,
                        xlim.arg = NULL,
                        ylim.arg = NULL) {

  summary.arg = match.arg(summary.arg)

  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg,
                            group = group.arg))

  if (is.null(line.col.arg))
    p.tmp = p.tmp + geom_line(alpha = 0.25, size = 0.25)
  else
    p.tmp = p.tmp + geom_line(aes_string(colour = line.col.arg),
                              alpha = 0.5,
                              size = 0.5)


  switch(summary.arg,
         none = {p.tmp = p.tmp },
         mean = {
           p.tmp = p.tmp +
             stat_summary(
               aes_string(y = y.arg, group = 1),
               fun.y = mean,
               colour = 'red',
               linetype = 'solid',
               size = 1,
               geom = "line",
               group = 1
             )},
         meanCInorm = {    p.tmp = p.tmp +
           stat_summary(
             aes_string(y = y.arg, group = 1),
             geom = "ribbon",
             fun.data = mean_cl_normal,
             colour = 'red',
             alpha = 0.5,
             group = 1
           ) +
           stat_summary(
             aes_string(y = y.arg, group = 1),
             fun.y = mean,
             colour = 'red',
             linetype = 'solid',
             size = 1,
             geom = "line",
             group = 1
           )
         },
         meanCIboot = {    p.tmp = p.tmp +
           stat_summary(
             aes_string(y = y.arg, group = 1),
             geom = "ribbon",
             fun.data = mean_cl_boot,
             colour = 'red',
             alpha = 0.5,
             group = 1
           ) +
           stat_summary(
             aes_string(y = y.arg, group = 1),
             fun.y = mean,
             colour = 'red',
             linetype = 'solid',
             size = 1,
             geom = "line",
             group = 1
           )
         })


  if (!is.null(facet.arg))
    p.tmp = p.tmp +
      facet_wrap(
        as.formula(paste("~", facet.arg)),
        ncol = facet.ncol.arg,
        scales = "free_x",
        drop = FALSE
      )

  if (!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_line(
      data = dt.stim.arg,
      aes_string(x = stim.x.arg, y = stim.y.arg),
      colour = 'blue',
      size = 1,
      group = 1
    )
  }

  if (!is.null(ylim.arg))
    p.tmp = p.tmp +
      coord_cartesian(ylim = ylim.arg)

  if (!is.null(xlim.arg))
    p.tmp = p.tmp +
      coord_cartesian(xlim = xlim.arg)

  p.tmp = p.tmp +
    scale_x_continuous(breaks = seq(0, maxrt.arg, xaxisbreaks.arg)) +
    xlab(paste0(xlab.arg, "\n")) +
    ylab(paste0("\n", ylab.arg)) +
    ggtitle(plotlab.arg) +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text.x = element_text(size = 14, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"),
      legend.position = "top"
    )

  return(p.tmp)
}
