river <- sf::st_sfc(sf::st_linestring(cbind(c(-6, 6), c(0, 0))))

test_that("Candidate segments boundaries are properly grouped and filtered", {
  e1 <- sf::st_linestring(cbind(c(-3, -3), c(-1, 1)))  # group 1 <--
  e2 <- sf::st_linestring(cbind(c(-3.1, -2.9), c(-1, 1)))  # group 1
  e3 <- sf::st_linestring(cbind(c(-2, -1.8), c(-1, 1)))  # group 2
  e4 <- sf::st_linestring(cbind(c(-2, -2), c(-1, 1)))  # group 2 <--
  e5 <- sf::st_linestring(cbind(c(-0.5, 0.5), c(-1, 1)))  # group 3 <--
  e6 <- sf::st_linestring(cbind(c(0.5, -0.5), c(-1, 1)))  # group 3
  e7 <- sf::st_linestring(cbind(c(1, 1), c(-1, 1)))  #  group 4 <--
  crossings <- sf::st_sfc(e1, e2, e3, e4, e5, e6, e7)
  expected <- sf::st_sfc(e1, e4, e5, e7)
  actual <- filter_clusters(crossings, river, eps = 0.2)
  expect_setequal(expected, actual)
})

test_that("Candidate segments are properly refined", {
  # A single solution is possible for each of the methods
  p1 <- sf::st_polygon(list(cbind(c(-4, -4, -3, -3, -4),
                                  c(-1, 1, 1, -1, -1))))
  p2 <- sf::st_polygon(list(cbind(c(-3, -3, -2, -2, -3),
                                  c(0.9, 1, 1, 0.9, 0.9))))
  p3 <- sf::st_polygon(list(cbind(c(-3, -3, -2, -2, -3),
                                  c(-1, 0.9, 0.9, -1, -1))))
  p4 <- sf::st_polygon(list(cbind(c(-2, -2, -1, -1, -2),
                                  c(0.9, 1, 1, 0.9, 0.9))))
  p5 <- sf::st_polygon(list(cbind(c(-2, -2, -1, -1, -2),
                                  c(-1, 0.9, 0.9, -1, -1))))
  p6 <- sf::st_polygon(list(cbind(c(-1, -1, 4, 4, -1),
                                  c(-1, 1, 1, -1, -1))))
  blocks <- sf::st_sfc(p1, p2, p3, p4, p5, p6)
  to_merge <- c(2, 4)
  # p2 shares the longest intersection with p3, and p4 with p5
  expected_longest_intersection <- sf::st_sfc(p1, p6,
                                              sf::st_union(p2, p3),
                                              sf::st_union(p4, p5))
  actual_longest_intersection <- merge_blocks(blocks, to_merge,
                                              "longest-intersection")
  # p2 and p4 are each others' smallest neighbours
  expected_smallest <- sf::st_sfc(p1, p3, p5, p6, sf::st_union(p2, p4))
  actual_smallest <- merge_blocks(blocks, to_merge, "smallest")
  equals_longest_intersection <- sf::st_equals(actual_longest_intersection,
                                               expected_longest_intersection,
                                               sparse = FALSE)
  equals_smallest <- sf::st_equals(actual_smallest, expected_smallest,
                                   sparse = FALSE)
  expect_true(all(sapply(seq_len(length(expected_longest_intersection)),
                         \(x) equals_longest_intersection[x, x])))
  expect_true(all(sapply(seq_len(length(expected_smallest)),
                         \(x) equals_smallest[x, x])))
})

test_that("Refinement works with equivalent options for merging", {
  p1 <- sf::st_polygon(list(cbind(c(-4, -4, -3, -3, -4),
                                  c(-1, 1, 1, -1, -1))))
  p2 <- sf::st_polygon(list(cbind(c(-3, -3, -2, -2, -3),
                                  c(0, 1, 1, 0, 0))))
  p3 <- sf::st_polygon(list(cbind(c(-3, -3, -2, -2, -3),
                                  c(-1, 0, 0, -1, -1))))
  p4 <- sf::st_polygon(list(cbind(c(-2, -2, -1, -1, -2),
                                  c(0, 1, 1, 0, 0))))
  p5 <- sf::st_polygon(list(cbind(c(-2, -2, -1, -1, -2),
                                  c(-1, 0, 0, -1, -1))))
  p6 <- sf::st_polygon(list(cbind(c(-1, -1, 4, 4, -1),
                                  c(-1, 1, 1, -1, -1))))
  blocks <- sf::st_sfc(p1, p2, p3, p4, p5, p6)
  to_merge <- c(2, 4)
  # p2 shares intersections of the same length with p1, p3 and p4,
  # it will thus be merged to the first element, i.e. p1.
  # p4 shares intersections of the same length with p2, p5 and p6,
  # After p2 is merged, the first neighbor in the list is p5.
  expected_longest_intersection <- sf::st_sfc(p3, p6,
                                              sf::st_union(p2, p1),
                                              sf::st_union(p4, p5))
  actual_longest_intersection <- merge_blocks(blocks, to_merge,
                                              "longest-intersection")
  # p2's neighbours p3 and p4 have the same size, it will thus be merged
  # to the first element, i.e. p3.
  # p4's neighbours p2 and p5 have the same size, after p2 is merged with p3
  # the remaining smallest element is p5.
  expected_smallest <- sf::st_sfc(p1, p6,
                                  sf::st_union(p2, p3), sf::st_union(p4, p5))
  actual_smallest <- merge_blocks(blocks, to_merge, "smallest")
  equals_longest_intersection <- sf::st_equals(actual_longest_intersection,
                                               expected_longest_intersection,
                                               sparse = FALSE)
  equals_smallest <- sf::st_equals(actual_smallest, expected_smallest,
                                   sparse = FALSE)
  expect_true(all(sapply(seq_len(length(expected_longest_intersection)),
                         \(x) equals_longest_intersection[x, x])))
  expect_true(all(sapply(seq_len(length(expected_smallest)),
                         \(x) equals_smallest[x, x])))
})
