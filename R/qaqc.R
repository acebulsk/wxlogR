# Quality Assurance and Quality Control

#' Function to Create a Complete Datetime Vector
#'
#' Takes a vector of POSIXct datetimes which potentially has gaps and will return a complete vector with no gaps. Unless timestep is entered it assumes the difference in the third and second record is the time step (usually the first record is at an odd time when the logger is turned on).
#'
#'
#' @param POSIXct datetime vector with potential gaps
#'
#' @return POSIXct vector
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
#' @examples df <- load_CS_1000(ex_path)
#' @examples datetime_seq_full(df$TIMESTAMP)
datetime_seq_full <- function(datetime, timestep = NA){
  date_range <- range(datetime, na.rm = T)

  if (is.na(timestep)) {
  timestep <- difftime(datetime[3], datetime[2], units = 'secs') |> as.numeric()
  }

  date_seq <- as.POSIXct(seq(date_range[1], date_range[2], by = timestep))

  return(date_seq)
}

#' Return Plot Showing Presence of Datetime Value as 1
#'
#' @param datetime POSIXct datetime vector with potential gaps
#' @param from POSIXct or Date or NA for min(datetime)
#' @param to POSIXct or Date or NA for max(datetime
#'
#' @return ggplot
#' @export
#'
#' @examples ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
#' @examples df <- load_CS_1000(ex_path)
#' @examples plot_data_gaps(df$TIMESTAMP)
plot_data_gaps <- function(datetime, from = NA, to = NA){

  if(is.na(from)){from <- min(datetime)}
  if(is.na(to)){to <- max(datetime)}

  full_seq <- wxlogR::datetime_seq_full(datetime)

  df_check <- data.frame(datetime = datetime, val = 1)
  df_full <- data.frame(datetime = full_seq)

  df_compare <- dplyr::left_join(df_full, df_check)

  g <- ggplot2::ggplot(df_compare |> dplyr::filter(datetime > from,
                                   datetime < to),
                       ggplot2::aes(datetime, val)) +
    ggplot2::geom_line()

  return(g)
}
