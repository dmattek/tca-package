#' Generate synthetic CellProfiler output with single cell time series
#'
#'
#'
#' @param in.ntpts Number of time points (default 60)
#' @param in.ntracks Number of tracks per FOV (default 10)
#' @param in.nfov Number of FOV (default 6)
#' @param in.nwells Number of wells (default 1)
#' @param in.addna Number of NAs to add randomly in the data (default NULL)
#'
#' @return Data table with the follwoing columns: Metadata_Site, Metadata_Well, Metadata_RealTime, objCyto_Intensity_MeanIntensity_imErkCor (normal distributed),
#' objNuc_Intensity_MeanIntensity_imErkCor (normal distributed), objNuc_Location_X and objNuc_Location_Y (uniform ditributed), TrackLabel
#' @export
#' @import data.table
#'
#' @examples
genTraj <- function(in.ntpts = 60, in.ntracks = 10, in.nfov = 6, in.nwells = 1, in.addna = NULL) {

  x.rand.1 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,   1, 0.2), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,  2, 0.5))
  x.rand.2 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.25, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.2),  rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 1, 0.2))

  # add NA's for testing
  if (!is.null(in.addna)) {
    locTabLen = length(x.rand.1)
    x.rand.1[round(runif(in.addna) * locTabLen)] = NA
    x.rand.2[round(runif(in.addna) * locTabLen)] = NA
  }

  x.arg = rep(seq(1, in.ntpts), in.ntracks * in.nfov)

  dt.nuc = data.table(Metadata_Well = rep(LETTERS[1:in.nwells], each = in.ntpts * in.nfov * in.ntracks / in.nwells),
                      Metadata_Site = rep(1:in.nfov, each = in.ntpts * in.ntracks),
                      Metadata_RealTime = x.arg,
                      objCyto_Intensity_MeanIntensity_imErkCor = x.rand.1,
                      objNuc_Intensity_MeanIntensity_imErkCor  = x.rand.2,
                      objNuc_Location_X = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      objNuc_Location_Y = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      TrackLabel = rep(1:(in.ntracks*in.nfov), each = in.ntpts))

  return(dt.nuc)
}
