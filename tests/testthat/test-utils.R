test_that("correct UTM zone is returend in the southern hemisphere", {
  city_name <- "Chennai, India"
  utm_from_location <-
    CRiSp::osmdata_as_sf("place", "city",
                         osmdata::getbb(city_name))$osm_polygons |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
    CRiSp::get_utm_zone_epsg()
  expect_equal(utm_from_location, 32644)
})

test_that("correct UTM zone is returend in the northern hemisphere", {
  city_name <- "Rejkjavik, Iceland"
  utm_from_location <-
    CRiSp::osmdata_as_sf("place", "city",
                         osmdata::getbb(city_name))$osm_polygons |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
    CRiSp::get_utm_zone_epsg()
  expect_equal(utm_from_location, 32627)
})
