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
  delineations <- suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                             corridor_init = "valley",
                                             corridor = TRUE))
  valley <- delineations$valley

  expect_true(inherits(valley, "sfc"))
  expect_true(inherits(valley, "sfc_POLYGON") ||
                inherits(valley, "sfc_MULTIPOLYGON"))
  expect_equal(length(valley), 1)
})

test_that("No valley is returned if buffer is used as initial corridor", {
  skip_on_ci()
  delineations <- suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                             corridor_init = 1500))
  expect_equal("corridor", names(delineations))

})
