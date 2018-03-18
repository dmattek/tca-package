#' Create segments of an ellipse around points on a 2D plane
#'
#' @param in.mat matrix with point coordinates. Rows correspond to points, columns to dimensions.
#' @param in.group vector with grouping. Must be of the same length as the number of rows in the input matrix.
#' @param in.col.sel vector to select columns from the input matrix. Must be of length 2.
#' @param in.scale scale factor to apply to observations (see param \code{obs.scale} in ggbiplot manual \url{https://www.rdocumentation.org/packages/ggbiplot/versions/0.55/topics/ggbiplot})
#' @param in.prob size of the ellipse in Normal probability (see param \code{ellipse.prob} in ggbiplot manual)
#'
#' @return data frame with first two columns with corrdinates of points of an ellipse, and column 'group' with groupings as provided by 'in.group' vector.
#' @export
#' @import plyr
#'
#' @examples
#' ## Draw red ellipse around scatter plot
#' n.pts = 50
#' n.cols = 2
#' m.gauss = matrix(rnorm(n.pts * n.cols), n.pts)
#' df.ell = ellipseFromScatter(m.gauss, rep(1, n.pts))
#' require(ggplot2)
#' ggplot(as.data.frame(m.gauss), aes(V1, V2)) +
#'   geom_point() +
#'   geom_path(data = df.ell, aes(X1, X2), color = 'red')
#'
#' ## Fill the ellipse with colour
#' require(dplyr)
#' df.hull <- df.ell %>%
#'   do(.[chull(.[1:2]), ])
#'
#' ggplot(as.data.frame(m.gauss), aes(V1, V2)) +
#'   geom_point() +
#'   geom_polygon(data = df.hull, aes(X1, X2), color = 'red', fill = 'red', alpha = 0.5)
#'
ellipseFromScatter = function(in.mat, in.group, in.col.sel = 1:2, in.scale = 1, in.prob = 0.68) {

  if(ncol(in.mat) < 2)
    stop('Input matrix must have at least two columns.')

  if (length(in.col.sel) != 2)
    stop('Vector with column numbers must be of length 2.')

  if(nrow(in.mat) != length(in.group))
    stop('Length of grouping vector must be the same as the number of rows in the input matrix.')

  # store names of selected columns
  loc.colnames = colnames(in.mat[, in.col.sel])

  # calculate SD along columns of the input matrix
  loc.sd = apply(in.mat[, in.col.sel], 2, sd)

  loc.nobs.factor = sqrt(nrow(in.mat) - 1)
  loc.u = sweep(in.mat[, in.col.sel], 2, 1/(loc.sd * loc.nobs.factor), FUN = "*")
  loc.df.u <- as.data.frame(sweep(loc.u, 2, loc.sd^in.scale, FUN = "*"))
  names(loc.df.u) <- c("xvar", "yvar")

  loc.df.u <- loc.df.u * loc.nobs.factor
  loc.df.u$group <- in.group

  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))

  loc.ell <- ddply(loc.df.u, "group", function(x) {
    if (nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(in.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = "+"), group = x$group[1])
  })

  # assign original names of columns with coordinates
  if(!is.null(loc.colnames))
    names(loc.ell)[1:2] = loc.colnames

  return(loc.ell)
}
