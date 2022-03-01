# Scripts related to Eddy Covariance loading

#' Load Campbell Sci hi frequency (HF) data, if header is TOB3 need to convert
#' to TOA5 using card convert. Uses data.table package for loading for increased
#' performance. Should load 5 gigs in a few seconds.
#'
#' @param path A path to the data readout to be loaded.
#' @param datetime String to be used for the datetime column.
#' @param timezone The POSIXct timezone for original readout.
#' @param col_names Vector of column names same length as number of columns in the readout.
#' @param skip_4_names Number of lines to skip in the readout to get to column names. Default = 1.
#' @param skip_4_dat Number of lines to skip in the readout to get data. Default = 3.
#' @param num_rows Number of rows to retrieve.
#'
#' @return
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'HF_EC_example.dat', package = 'wxlogR')
#' @examples df <- load_CS_HF(ex_path)
load_CS_HF <- function(path, datetime = 'TIMESTAMP', timezone = 'EST', col_names = NA, skip_4_names = 1, skip_4_dat = 4, num_rows = 1000){

  if (is.na(col_names) == T) {
    col_names <- names(data.table::fread(path, nrows = 1, skip = skip_4_names))
  } else if(is.vector(col_names) == F) {
    stop("ERROR: col_names must be a vector.")
  }

  df <- data.table::fread(path, skip = skip_4_dat, col.names = col_names, nrows = num_rows)

  # data.table defaults to UTC so need to change back to what we think the timezone actually is
  df[, datetime] <- as.POSIXct(as.character(df[[datetime]]), tz = timezone)

  return(df)
}
