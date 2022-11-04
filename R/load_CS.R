# Scripts related to usual campbell sci readouts

#' Load Campbell Sci formatted logger readout. Default is setup to work with 1000x / 3000x format.
#'
#' @param path A path to the data readout to be loaded.
#' @param datetime String to be used for the datetime column.
#' @param timezone POSIXct timezone for readout. Default to UTC so we dont have timezone conversion issue.
#' @param col_names Vector of column names same length as number of columns in the readout.
#' @param skip_4_names Number of lines to skip in the readout to get to column names. Default = 1.
#' @param skip_4_dat Number of lines to skip in the readout to get data. Default = 3.
#' @param na_string Character string that represents NULL value
#' @return dataframe
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
#' @examples df <- load_CS_1000(ex_path)
load_CS_1000 <- function(path, datetime = 'TIMESTAMP', timezone = 'UTC', col_names = NA, skip_4_names = 1, skip_4_dat = 3, na_string = "NAN"){

  if (is.na(col_names) == T) {
    col_names <- colnames(read.csv(path, skip = skip_4_names))
  } else if(is.vector(col_names) == F) {
    stop("ERROR: df_met_nms must be a vector.")
  }

  df <- utils::read.csv(path, skip = skip_4_dat, col.names = col_names, na.strings = na_string)

  df[, datetime] <- as.POSIXct(df[, datetime], tz = timezone)

  return(df)
}

#' Load Campbell Sci 10x logger readout. Usually no column names here so need to input yourself.
#'
#' @param path A path to the data readout to be loaded.
#' @param timezone POSIXct timezone for readout. Default to UTC so we dont have timezone conversion issue.
#' @param col_names Vector of column names same length as number of columns in the readout.
#' @param pos_year Column position for year.
#' @param pos_doy Column position for day of year.
#' @param pos_time Column position for CS time. Formatted as (ex. 100 == 01:00:00 a.m. 60 minutes since midnight)
#' @param index what is the row index you are after. Default is 115 which is the 15 min average and removes the daily average.
#'
#' @return logger readout with datetime parsed and with column headers and daily average removed
#' @export
#'
#' @examples ex_path <- system.file('extdata', '10x.dat', package = 'wxlogR')
#' @examples df <- load_CS_10(ex_path)
load_CS_10 <- function(path, timezone = 'UTC', col_names = NA, pos_year = 2, pos_doy = 3, pos_time = 4, index = 115) {

  if (any(is.na(col_names)) == T){
    df <- utils::read.csv(path, header = F)
  } else {
    df <- utils::read.csv(path, col.names = col_names, header = F)
  }

  # remove daily average row, keep row we want 115

  df <- df[df[,1] == 115,]

  df$date <- as.Date(df[,pos_doy]-1, origin = paste0(df[,pos_year],'-01-01'))

  df$datetime <- as.POSIXct(paste(df$date, parse_CS_time(df[, pos_time])), format = '%Y-%m-%d %H%M%S', tz = timezone)

  return(df)
}
