
# bcg.annotation

<!-- badges: start -->
<!-- badges: end -->

bcg.annotation is an interactive tool for manually annotating heart beats in ballistocardiogram data.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FlukeAndFeather/bcg.annotation")
```

## Example

Run the annotation app on the sample data included with the package:

``` r
library(bcg.annotation)
# Location of example data
nc_path <- system.file("extdata/bw180905-53_prh10_ballisto.nc", package = "bcg.annotation")
# Copy the data to somewhere convenient
file.copy(nc_path, "~/Downloads/")
# Run app and follow instructions in sidebar
bcg_app()
```

## Issues

If you encounter a bug (crash or otherwise) or issues with installation, please create an issue!
