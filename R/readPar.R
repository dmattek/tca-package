#' Check whether a string consists only from digits
#'
#' @param x Input string
#'
#' @return True if the input string consists only from digits, False otherwise.
#' @export
#'
#' @examples
#' checkDigits('1111')
#' checkDigits('1111cccc')
checkDigits <- function(x) {
  grepl('^[-]?[0-9]+[.]?[0-9]*$' , x)
}

#' Check whether a string matches TRUE/FALSE, T/F, or T.../F...
#'
#' @param x Input string
#'
#' @return True if the input string matches the pattern, False otherwise.
#' @export
#'
#' @examples
#' checkLogical('TRUE')
#' checkLogical('xxxTxxx')
checkLogical <- function(x) {
  grepl('^TRUE$|^FALSE$|^T$|^F$' , x)
}



#' Converts string elements of a named list to apporpriate types
#'
#' Strings that consist of digits are converted to type \code{numeric}, strings with TRUE/FALSE, T/F, or T.../F... to \code{logical}.
#'
#' @param in.l Named list fo strings.
#'
#' @return Named list with elements converted to appropriate types.
#' @export
#' @import xlsx
#'
#' @examples
#'  l.tst = list()
#'  l.tst$aaa = '1000'
#'  l.tst$bbb = '1000xx'
#'  l.tst$ccc = 'True'
#'  l.tst$ddd = 'xxxTrue'
#'  l.res = convertStringListToTypes(l.tst)
#'  str(l.res)

convertStringList2Types <- function(in.l) {
  # convert strings with digits to numeric
  # uses logical indexing: http://stackoverflow.com/questions/42207235/replace-list-elements-by-name-with-another-list
  loc.l = checkDigits(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.numeric)

  # convert strings with TRUE/FALSE to logical
  loc.l = checkLogical(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.logical)

  return(in.l)
}


#' Return a list with parameter names and their values read from xlsx file
#'
#' The Excel xlsx file has to contain at least two columns:
#' 1st column with parameter names
#' 2nd column with parameter values
#'
#' In case of rJava error when loading, run: sudo R CMD javareconf
#'
#' @param in.fname Name of the xlsx file.
#' @param in.cols Vector with column names to read. Has to be of length 2.
#' @param in.sheet.idx Integer with the sheet number in the xlsx to process.
#'
#' @return Named list with parameters and their values.
#' @export

readPar = function(in.fname, in.cols = 1:2, in.sheet.idx = 1) {

  if(length(in.cols) != 2)
    stop('Parameter in.cols has to be of length 2.')

  df.par = read.xlsx(
    in.fname,
    sheetIndex = in.sheet.idx,
    header = FALSE,
    as.data.frame = TRUE,
    colIndex = in.cols,
    colClasses = rep("character", 2),
    stringsAsFactors = FALSE
  )

  # convert data frame with parameters to a list
  l.par = split(df.par[, 2], df.par[, 1])

  # convert strings with digits to numeric and strings with TRUE/FALSE to logical
  l.par = convertStringList2Types(l.par)

  return(l.par)
}
