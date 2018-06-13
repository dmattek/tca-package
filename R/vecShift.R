#' Shift vector values to right or left
#' Modified from: https://stackoverflow.com/a/37069049/1898713
#'
#' @param in.x Vector for which to shift values
#' @param in.n Number of places to be shifted.
#'    Positive numbers will shift to the right by default.
#'    Negative numbers will shift to the left by default.
#'    The direction can be inverted by the invert parameter.
#' @param in.invert Whether or not the default shift directions
#'    should be inverted.
#' @param in.fill The value that should be inserted by default.
#' @param in.circle If TRUE, inserted values will be taken from shifted vector
#'
#' @return Shifted vector
#'
#' @examples
#' v = 1:10
#'
#' # Shift by two elements right, fill with NAs
#' vecShift(v, 2)
#'
#' # Shift by two elements right, fill with 0's
#' vecShift(v, 2, in.fill = 0)
#'
#' # Shift by two elements left, fill circular
#' vecShift(v, -2, in.circle = T)
#'


vecShift <- function(in.x, in.n, in.invert=FALSE, in.fill=NA, in.circle = F){
  stopifnot(length(in.x) >= in.n)

  if(in.n==0){
    return(in.x)
  }

  in.n <- ifelse(in.invert, in.n*(-1), in.n)

  if(in.n<0){
    in.n <- abs(in.n)
    loc.forward=FALSE
  }else{
    loc.forward=TRUE
  }

  if (in.circle) {
    # Circular fill
    if(loc.forward){
      loc.fill = loc.x[seq(length(in.x) - in.n + 1, length(in.x))]
      return(c(loc.fill, in.x[seq_len(length(in.x)-in.n)]))
    } else {
      loc.fill = in.x[seq(1, in.n)]
      return(c(in.x[seq_len(length(in.x)-in.n)+in.n], loc.fill))
    }
  } else {
    # Fill with a supplied value
    if(loc.forward){
      return(c(rep(in.fill, in.n), in.x[seq_len(length(in.x)-in.n)]))
    } else {
      return(c(in.x[seq_len(length(in.x)-in.n)+in.n], rep(in.fill, in.n)))
    }
  }
}
