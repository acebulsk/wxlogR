# Quality Assurance and Quality Control

#' Spike Detection
#'
#' @param data dataframe with data column of interest
#' @param datetime col for datetime to fill gaps
#' @param col_var col name for snow depth
#' @param threshold threshold to be used for pos. AND neg. spikes
#' @param roc_hi_th rate of change threshold
#' @param roc_low_th rate of change threshold
#' @param pos_neg string 'pos' OR 'neg' or 'both' to look at only high, low or both spike directions
#'
#' @return cleaned dataframe
#' @export
#'
spike_clean <- function(data, datetime, col_var, threshold, roc_hi_th, roc_low_th, pos_neg = 'both') {
  if (nrow(data) == 0) {
    stop("Error: missing data values")
  }

  if (pos_neg == 'pos'){
    diffs <- diff(data[, col_var])
    spikes <- diffs > threshold
    numSpikes <- sum(spikes)

    if (numSpikes == 0) {
      outputMessage <- " No spikes found"
      returnvalue <- 0
    }

    else {
      locs <- obs[spikes, 1][-1]
      outputMessage <- paste(" ", numSpikes, " spikes found", sep = "")
      returnvalue <- locs
    }

  }

  data <- tsibble::as_tsibble(data, index = !!rlang::sym(datetime)) |>
    tsibble::fill_gaps() |>
    dplyr::mutate(
      snow_clean =
        dplyr::case_when(
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow)) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow), n = 2) - !!rlang::sym(snow)) > spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  low spike
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) > spike_th & (dplyr::lead(!!rlang::sym(snow)) - !!rlang::sym(snow)) < -spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  hi spike
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow)) - !!rlang::sym(snow)) > spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  low spike
          TRUE ~ !!rlang::sym(snow) # else set to raw
        )
    )

  # while the dataset does not have a rate of change higher than the threshold
  while(TRUE %in% (data$snow_clean - dplyr::lag(data$snow_clean) > roc_hi_th)){
    data <- data |>
      dplyr::mutate(
        snow_clean =
          dplyr::case_when(

            (snow_clean - dplyr::lag(snow_clean)) > roc_hi_th ~ dplyr::lag(snow_clean), # rate of change
            (snow_clean - dplyr::lead(snow_clean)) > roc_low_th ~ dplyr::lead(snow_clean), # rate of change
            TRUE ~ snow_clean # else set to raw
          )
      )
  }
  return(data)
}

#' Function to Create a Complete Datetime Vector
#'
#' Takes a vector of POSIXct datetimes which potentially has gaps and will return a complete vector with no gaps. Unless timestep is entered it assumes the difference in the third and second record is the time step (usually the first record is at an odd time when the logger is turned on).
#'
#'
#' @param POSIXct datetime vector with potential gaps
#' @param timestep timestep in seconds (numeric)
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

#' Sets new values for where flags exist
#'
#' @param df dataframe, note cannot be a tibble and will error out due to the base filtering sytax used here.
#' @param col_val string or vector of strings referring to dataframe column of the raw values to be updated.
#' @param col_flag string referring to the dataframe column of flags.
#' @param bad_flag vector of bad flags, type is same as the col_flag col.
#' @param flag_val value to insert in col_val where is flagged.
#'
#' @return NA, updates existing col_val column in df.
#' @export
#'
#' @examples
qc_data_filter <- function(df, col_val, col_flag, bad_flags, flag_val) {
  stopifnot(tibble::is_tibble(df) == F)
  warning('make sure we are using bad_flags as this function has recently changed from accepting good flags.')
  df[df[,col_flag] %in% bad_flags, col_val] <- flag_val
  return(df)
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

  if(is.na(from)){from <- min(datetime, na.rm = T)}
  if(is.na(to)){to <- max(datetime, na.rm = T)}

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

#' Use the data editor for a CSV file
#'
#' This function loads a CSV file, lets the user edit it in the native data
#' editor, then re-saves it, prompting the user for a new file name if desired.
#'
#' @param file string of filename
#' @param new.name T/F
fix_csv <- function(file, new.name=TRUE) {
  tmpframe <- read.csv(file)
  tmpframe <- edit(tmpframe)
  if(is.character(new.name)) {
    out.name <- new.name
  } else if(new.name <- TRUE) {
    out.name <- readline(prompt="Enter file name to save (Hit enter to use original):")
  } else {
    out.name <- file
  }
  if(out.name=="") out.name <- file
  write.table(tmpframe, file=out.name, append=FALSE, quote=FALSE, sep=sep,
              row.names=FALSE)
}
