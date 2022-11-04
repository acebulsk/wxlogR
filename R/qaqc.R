# Quality Assurance and Quality Control

#' Function to Create a Complete Datetime Vector
#'
#' Takes a vector of POSIXct datetimes which potentially has gaps and will return a complete vector with no gaps. Unless timestep is entered it assumes the difference in the third and second record is the time step (usually the first record is at an odd time when the logger is turned on).
#'
#'
#' @param datetime
#'
#' @return POSIXct vector
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
#' @examples df <- load_CS_HF(ex_path)
#' @examples datetime_seq_full(df$datetime)
datetime_seq_full <- function(datetime, timestep = NA){
  date_range <- range(datetime)

  if (is.na(timestep)) {
  timestep <- difftime(datetime[3], datetime[2], units = 'secs') |> as.numeric()
  }

  date_seq <- as.POSIXct(seq(date_range[1], date_range[2], by = timestep))

  return(date_seq)
}
