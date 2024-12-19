#' Parse Campbell Sci 10x time.
#'
#' @param time formatted as (ex. 100 == 01:00:00 a.m. 60 minutes since midnight)
#'
#' @return time integer formatted so that posixct can accept (HHMMSS)
#' @export
#'
#' @examples parse_CS_time('100')
parse_CS_time <- function(time) {
  formatC(as.numeric(time) * 100, width = 6, format = "d", flag = "0")
}
