#' @title Extract latest O2 values
#' 
#' @description Extracts the last O2 values from a PreSens text file.
#' 
#' @param file a character string. The filepath for the file to be read.
#' @param n_last integer. The number of O2 values to extract and return. Default is 10.
#'
#' @return A vector of numeric O2 values with a length of \code{n_last}.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{import_o2}}
#' 
#' @examples
#' \dontrun{
#' file <- system.file('extdata', 'all_o2_units.txt', package = 'presens')
#' last_o2(file)
#' last_o2(file, n_last = 5)
#' }
#' 
#' @encoding UTF-8
#' @export
#' @import utils

last_o2 = function(file, n_last = 10)
{
  f = utils::tail(try(readLines(file)), n_last)
  f = gsub(pattern = ' ', replacement = '', f)
  f = strsplit(f, split = ';')
  last = sapply(f, '[', 4)
  last = as.numeric(last)
  return(last)
}