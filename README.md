
# wxlogR

<!-- badges: start -->
<!-- badges: end -->

This is an R package containing functions to simplify the processing of hydrometerological station data. Current support is for Campbell Sci loggers but will eventually support FTS and HOBO loggers as well.

## Installation

You can install the development version of wxlogR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("acebulsk/wxlogR")
```

## Example

This is a basic example of how to load the package and process a CR1000x file. The load_CS_1000 function handles column naming and datetime formatting.

``` r
library(wxlogR)
## basic example code 

met_path <- 'data/path_to_your_1000x_file.dat'

met_data <- load_CS_1000(met_path)

## example file format

example_path <- system.file('extdata', 'treefort_1000x.dat', package = 'wxlogR')

met_raw <- read.csv(example_path)

met_data <- load_CS_1000(example_path)
```

