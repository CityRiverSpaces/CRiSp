test_that("proper parameters must be provided depending on selected method", {
  river <- bucharest_osm$river_centerline

  # for "buffer" method, we need the "buffer" parameter
  expect_error(initial_corridor(river, method = "buffer"),
               "Buffer should be provided if `method` is `'buffer'`")
  with_mocked_bindings(river_buffer = function(...) NULL, {
    expect_no_error(initial_corridor(river, method = "buffer", buffer = 42))
  })

  # for "valley" method, we need the "dem" parameter
  expect_error(initial_corridor(river, method = "valley"),
               "DEM should be provided if `method` is `'valley'`")
  with_mocked_bindings(get_valley = function(...) NULL, {
    expect_no_error(initial_corridor(river, method = "valley", dem = 42))
  })

  # inexistent method raise an error
  expect_error(initial_corridor(river, method = "crisp"),
               "Unknown method to initialize river corridor: crisp")
})

test_that("Build river network works with multiple linestring features", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expect_setequal(river, actual_edges)
})

test_that("Build river network does not simplify loops", {
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-2, -1), c(0, 0))),
    sf::st_linestring(cbind(c(-1, 0, 0), c(0, 0, -1))),
    sf::st_linestring(cbind(c(0, 0), c(-1, -2))),
    sf::st_linestring(cbind(c(-1, -1, 0), c(0, -1, -1)))
  )
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expect_setequal(river, actual_edges)
})

test_that("Build river network also works with multilinestrings", {
  river <- sf::st_sfc(c(
    sf::st_linestring(cbind(c(-2, -1), c(0, 0))),
    sf::st_linestring(cbind(c(-1, 0, 0), c(0, 0, -1))),
    sf::st_linestring(cbind(c(-1, -1, 0), c(0, -1, -1))),
    sf::st_linestring(cbind(c(0, 0), c(-1, -2)))
  ))
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expected_edges <- sf::st_cast(river, "LINESTRING")
  expect_setequal(expected_edges, actual_edges)
})

test_that("Build river network only select longest segment within AoI", {
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-2, 0, 0, -1), c(0, 0, -1, -1)))
  )
  bbox <- sf::st_bbox(c(xmin = -1.5, ymin = -2, xmax = -0.5, ymax = 2))
  river_network <- build_river_network(river, aoi = bbox)
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expected_edges <- sf::st_sfc(sf::st_linestring(cbind(c(-1.5, -0.5), c(0, 0))))
  expect_setequal(expected_edges, actual_edges)
})

test_that("Endpoints are found for two intersections with network edges", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5))),
    sf::st_linestring(cbind(c(1, -1), c(0.5, 0.5))),
    sf::st_linestring(cbind(c(0.5, 0.5), c(1, -1))),
    sf::st_linestring(cbind(c(-0.5, -0.5), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  actual <- corridor_end_points(river_network, spatial_network)
  expected <- sf::st_sfc(sf::st_point(c(0, -0.5)), sf::st_point(c(-0.5, 0)))
  expect_setequal(actual, expected)
})

test_that("Endpoints are found for more intersections with network edges", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5))),
    sf::st_linestring(cbind(c(1, -1), c(-0.25, -0.25))),
    sf::st_linestring(cbind(c(-0.25, -0.25), c(1, -1))),
    sf::st_linestring(cbind(c(-0.5, -0.5), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  actual <- corridor_end_points(river_network, spatial_network)
  expected <- sf::st_sfc(sf::st_point(c(0, -0.5)), sf::st_point(c(-0.5, 0)))
  expect_setequal(actual, expected)
})

test_that("An error is raised for a single intersection with network edge", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5))),
    sf::st_linestring(cbind(c(0.5, 0.5), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  expect_error(corridor_end_points(river_network, spatial_network), "coincide")
})

test_that("River banks for a simple river gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  regions <- get_river_banks(river, width = 1)
  expect_equal(length(regions), 2)
  expect_true(all(sf::st_geometry_type(regions) == "POLYGON"))
})

test_that("River banks for a more complex river still gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 2, 2, -2),
                                              c(0.5, 0.5, -0.75, -0.75))))
  regions <- get_river_banks(river, width = 2)
  expect_equal(length(regions), 2)
  expect_true(all(sf::st_geometry_type(regions) == "POLYGON"))

})

test_that("River banks works with real data", {
  river <- bucharest_osm$river_centerline
  regions <- get_river_banks(river, width = 2500)
  expect_equal(length(regions), 2)
})

test_that("Initial edges are identified if corridor exceeds AoI", {
  #        ____________
  #       |            |
  #       | north bank |
  #  _____|_ _ _ _ _ _ |_____
  # |     |____________|     | <- corridor
  # |_____|_ _ _ _ _ _ |_____|
  #       |            |
  #       | south bank |
  #       |____________|
  #
  # the dashed lines are the expected initial corridor edges
  corridor <- sf::st_as_sfc(sf::st_bbox(c(xmin = -2, xmax = 2,
                                          ymin = -0.5, ymax = 0.5)))
  north_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = 0, ymax = 1))
  south_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 0))
  regions <- c(sf::st_as_sfc(north_bank), sf::st_as_sfc(south_bank))
  edges_actual <- initial_edges(corridor, regions)
  edges_expected <- sf::st_sfc(
    sf::st_linestring(cbind(c(-1, 1), c(0.5, 0.5))),
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5)))
  )
  expect_setequal(edges_actual, edges_expected)
})

test_that("Initial edges are identified if AoI includes corridor", {
  #  ________________________
  # |                        |
  # |       north bank       |
  # |      _ _ _ _ _ _       |
  # |     |  corridor  |     |
  # |_____|____________|_____|
  # |     |            |     |
  # |     |_ _ _ _ _ _ |     |
  # |                        |
  # |       south bank       |
  # |________________________|
  #
  # the dashed lines are the expected initial corridor edges
  corridor <- sf::st_as_sfc(sf::st_bbox(c(xmin = -0.5, xmax = 0.5,
                                          ymin = -0.5, ymax = 0.5)))
  north_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = 0, ymax = 1))
  south_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 0))
  regions <- c(sf::st_as_sfc(north_bank), sf::st_as_sfc(south_bank))
  edges_actual <- initial_edges(corridor, regions)
  edges_expected <- sf::st_sfc(
    sf::st_linestring(cbind(c(-0.5, -0.5, 0.5, 0.5), c(0, 0.5, 0.5, 0))),
    sf::st_linestring(cbind(c(0.5, 0.5, -0.5, -0.5), c(0, -0.5, -0.5, 0)))
  )
  expect_setequal(edges_actual, edges_expected)
})

test_that("Capping a corridor with method 'direct' properly closes a polygon", {
  edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
  edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
  edges <- sf::st_sfc(edge_1, edge_2)
  corridor <- cap_corridor(edges, method = "direct")
  pts_corridor <- sf::st_cast(corridor, "POINT")
  pts_edges <- sf::st_cast(edges, "POINT")
  # we verify that the corridor includes only points coming from the edges
  expect_setequal(pts_corridor, pts_edges)
})

test_that("Capping a corridor with 'shortest_path' uses network paths", {
  #    p2 -- p3
  #  /         \
  # p1          p4
  #  \         /
  #    p6 -- p5
  nodes <- sf::st_sfc(
    sf::st_point(c(-2, 0)),
    sf::st_point(c(-1, 1)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 0)),
    sf::st_point(c(1, -1)),
    sf::st_point(c(-1, -1))
  )
  edges <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(c(nodes[[1]], nodes[[2]])),
    sf::st_linestring(c(nodes[[2]], nodes[[3]])),
    sf::st_linestring(c(nodes[[3]], nodes[[4]])),
    sf::st_linestring(c(nodes[[4]], nodes[[5]])),
    sf::st_linestring(c(nodes[[5]], nodes[[6]])),
    sf::st_linestring(c(nodes[[6]], nodes[[1]]))
  ))
  edges$from <- c(1, 2, 3, 4, 5, 6)
  edges$to <- c(2, 3, 4, 5, 6, 1)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  corridor_edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
  corridor_edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
  corridor_edges <- sf::st_sfc(corridor_edge_1, corridor_edge_2)
  corridor <- cap_corridor(corridor_edges, method = "shortest-path",
                           network = network)
  pts_corridor <- sf::st_cast(corridor, "POINT")
  # we verify that the corridor includes all the nodes of the network
  expect_setequal(pts_corridor, nodes)
})
