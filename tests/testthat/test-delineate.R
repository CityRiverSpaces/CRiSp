test_that("Segmentation without corridor raises error", {
  skip_on_ci()
  expect_error(delineate("Bucharest", "Dâmbovița",
                         corridor = FALSE, segment = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Default network buffer is used with message", {
  skip_on_ci()
  expect_message(suppressWarnings(delineate("Bucharest", "Dâmbovița")),
                 paste("The default `network_buffer` of 3000 m is used",
                       "for corridor delineation."))
})

test_that("Default buildings buffer is used for riverspace delineation
          with message", {
            skip_on_ci()
            expect_message(suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                                      riverspace = TRUE)),
                           paste("The default `buildings_buffer` of 100 m is",
                                 "used for riverspace delineation."))
          })

test_that("Valley is returned", {
  skip_on_ci()

  valley <- suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                       valley = TRUE,
                                       corridor = TRUE,
                                       segments = TRUE,
                                       riverspace = TRUE))$valley

  expect_true(inherits(valley, "sfc"))
  expect_true(inherits(valley, "sfc_POLYGON") ||
                inherits(valley, "sfc_MULTIPOLYGON"))
  expect_equal(length(valley), 1)
})

test_that("Throw error if valley is requested with wrong initial_method", {
  skip_on_ci()
  expect_error(suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                          initial_method = "buffer",
                                          valley = TRUE)),
               paste("The valley can only be returned if `initial_method` is",
                     "`\"valley\"`"))
})
