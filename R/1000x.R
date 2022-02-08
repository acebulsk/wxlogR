#' Title
#'
#' @param path A path to the data readout to be loaded.
#' @param datetime String to be used for the datetime column.
#' @param timezone POSIXct timezone for readout.
#' @param col_names Vector of column names same length as number of columns in the readout.
#' @param skip_4_names Number of lines to skip in the readout to get to column names.
#' @param skip_4_dat Number of lines to skip in the readout to get data.
#'
#' @return
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
#' @examples df <- load_1000x(ex_path)
load_1000x <- function(path, datetime = 'TIMESTAMP', timezone = 'EST', col_names = NA, skip_4_names = 1, skip_4_dat = 3){

  if (is.na(col_names) == T) {
    col_names <- colnames(read.csv(path, skip = skip_4_names))
  } else if(is.vector(col_names) == F) {
    stop("ERROR: df_met_nms must be a vector.")
  }

  df <- utils::read.csv(path, skip = skip_4_dat, col.names = col_names)

  df[, datetime] <- as.POSIXct(df[, datetime], tz = timezone)

  return(df)
}
