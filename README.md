
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CRiSp

<!-- badges: start -->

[![R-CMD-check](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

CRiSp (City River Spaces) provides tools to automate the morphological
delineation of riverside urban areas.

## Installation

You can install the released version of CRiSp from
[CRAN](https://cran.r-project.org) with:

``` r
install.packages("CRiSp")
```

You can install the development version of CRiSp from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("CityRiverSpaces/CRiSp")
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
bb <- get_osm_bb(city_name)
streets <- get_osm_streets(bb, epsg_code)$geometry
railways <- get_osm_railways(bb, epsg_code)$geometry

# Delineate river corridor
corridor <- delineate(city_name, river_name)$corridor

# Plot results
plot(corridor, border = "orange", lwd = 3)
plot(railways, add = TRUE, lwd = 0.5)
plot(streets, add = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contributing

We look very much forward to contributions to the package. See the
[Contributing Guide](.github/CONTRIBUTING.md) for further details.

This package is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.
