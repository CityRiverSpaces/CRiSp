
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CRiSp

<!-- badges: start -->

[![R-CMD-check](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

CRiSp (City River Spaces) provides tools to automate the morphological
delineation of riverside urban areas.

## Installation

You can install the development version of CRiSp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CityRiverSpaces/CRiSp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CRiSp)

# Set location parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635

# Get base layer for plotting
bucharest_bb <- get_osm_bb(city_name) 
bucharest_streets <- get_osm_streets(bucharest_bb, epsg_code)[, "geometry"]

# Delineate river corridor
bucharest_river <- delineate_corridor("Bucharest", "Dâmbovița", epsg_code)
#> Warning: to_spatial_subdivision assumes attributes are constant over geometries
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

# Plot results
plot(bucharest_river, border = "orange", lwd = 3)
plot(bucharest_streets, add = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contributing

We look very much forward to contributions to the package. See the
[Contributing Guide](.github/CONTRIBUTING.md) for further details.

This package is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.

