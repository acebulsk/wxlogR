% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qaqc.R
\name{datetime_seq_full}
\alias{datetime_seq_full}
\title{Function to Create a Complete Datetime Vector}
\usage{
datetime_seq_full(datetime, timestep = NA)
}
\arguments{
\item{POSIXct}{datetime vector with potential gaps}
}
\value{
POSIXct vector
}
\description{
Takes a vector of POSIXct datetimes which potentially has gaps and will return a complete vector with no gaps. Unless timestep is entered it assumes the difference in the third and second record is the time step (usually the first record is at an odd time when the logger is turned on).
}
\examples{
ex_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')
df <- load_CS_1000(ex_path)
datetime_seq_full(df$TIMESTAMP)
}
