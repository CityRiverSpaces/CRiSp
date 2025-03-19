test_that("Segmentation without corridor raises error", {
  skip_on_ci()
  expect_error(delineate("Bucharest", "Dâmbovița",
                         corridor = FALSE, segment = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Default network buffer is used with message", {
  skip_on_ci()
  expect_message(suppressWarnings(delineate("Bucharest", "Dâmbovița")),
                 paste("The default `network_buffer` of 2500 m is used",
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
