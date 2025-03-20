#          p4
#          |
# p1 - p2 ---- p3
#          |
#         p5
p1 <- sf::st_point(c(0, 0))
p2 <- sf::st_point(c(1, 0))
p3 <- sf::st_point(c(3, 0))
p4 <- sf::st_point(c(2, 1))
p5 <- sf::st_point(c(2, -1))

e1 <- sf::st_linestring(c(p1, p2))
e2 <- sf::st_linestring(c(p2, p3))
e3 <- sf::st_linestring(c(p4, p5))

nodes <- sf::st_sfc(p1, p2, p3, p4, p5)
edges <- sf::st_as_sf(sf::st_sfc(e1, e2, e3))
edges$from <- c(1, 2, 4)
edges$to <- c(2, 3, 5)
network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                 directed = FALSE, force = TRUE,
                                 node_key = "x")

# p9 ----- p8
# |      / |
# |   p10  |
# |  /     |
# p6 ----- p7
p6 <- sf::st_point(c(-1, -1))
p7 <- sf::st_point(c(1, -1))
p8 <- sf::st_point(c(1, 1))
p9 <- sf::st_point(c(-1, 1))
p10 <- sf::st_point(c(0, 0))

e4 <- sf::st_linestring(c(p6, p7))
e5 <- sf::st_linestring(c(p7, p8))
e6 <- sf::st_linestring(c(p8, p9))
e7 <- sf::st_linestring(c(p9, p6))
e8 <- sf::st_linestring(c(p6, p10))
e9 <- sf::st_linestring(c(p8, p10))

nodes_shortpath <- sf::st_sfc(p6, p7, p8, p9, p10)
edges_shortpath <- sf::st_as_sf(sf::st_sfc(e4, e5, e6, e7, e8, e9))
edges_shortpath$from <- c(1, 2, 3, 4, 1, 3)
edges_shortpath$to <- c(2, 3, 4, 1, 5, 5)
edges_shortpath$length <- c(2, 2, 2, 2, sqrt(2) / 2, sqrt(2) / 2)
network_shortpath <- sfnetworks::sfnetwork(nodes = nodes_shortpath,
                                           edges = edges_shortpath,
                                           directed = FALSE, force = TRUE,
                                           node_key = "x")

#          p4
#          |
# p1 - p2  |
#          |
#          p5
nodes_no_crossings <- sf::st_sfc(p1, p2, p4, p5)
edges_no_crossings <- sf::st_as_sf(sf::st_sfc(e1, e3))
edges_no_crossings$from <- c(1, 3)
edges_no_crossings$to <- c(2, 4)
network_no_crossings <- sfnetworks::sfnetwork(nodes = nodes_no_crossings,
                                              edges = edges_no_crossings,
                                              directed = FALSE, force = TRUE,
                                              node_key = "x")

test_that("Network objects can be set up with no modifications", {
  edges <- sf::st_sfc(e1, e2, e3)
  network <- as_network(edges, flatten = FALSE, clean = FALSE)
  expect_true(inherits(network, "sfnetwork"))
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network, "nodes"))
  edges_actual <- sf::st_as_sf(network, "edges")
  nodes_expected <- sf::st_sfc(p1, p2, p3, p4, p5)
  from_expected <- c(1, 2, 4)
  to_expected <- c(2, 3, 5)
  expect_setequal(sf::st_geometry(edges_actual), edges)
  expect_setequal(edges_actual$from, from_expected)
  expect_setequal(edges_actual$to, to_expected)
  expect_setequal(nodes_actual, nodes_expected)
})

test_that("Network flattening inject intersection within edges", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  edges <- sf::st_as_sf(sf::st_sfc(e2, e3))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_flat <- flatten_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_flat, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_flat, "edges"))
  intersection <- sf::st_intersection(e2, e3)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p2, intersection, p3)),
                               sf::st_linestring(c(p4, intersection, p5)))
  expect_setequal(nodes_actual, nodes)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning transforms shared internal points to nodes", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  intersection <- sf::st_intersection(e2, e3)
  edges <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(c(p2, intersection, p3)),
                                   sf::st_linestring(c(p4, intersection, p5))))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p2, p3, p4, p5, intersection)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p2, intersection)),
                               sf::st_linestring(c(intersection, p3)),
                               sf::st_linestring(c(p4, intersection)),
                               sf::st_linestring(c(intersection, p5)))
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning drops pseudo nodes", {
  nodes <- sf::st_sfc(p1, p2, p3)
  edges <- sf::st_as_sf(sf::st_sfc(e1, e2))
  edges$from <- c(1, 2)
  edges$to <- c(2, 3)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p1, p3)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)))
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning drops disconnected components", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  edges <- sf::st_as_sf(sf::st_sfc(e2, e3))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p2, p3)
  edges_expected <- sf::st_sfc(e2)
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network simplification drops loops and multiple edges", {
  nodes <- sf::st_sfc(p2, p3)
  p6 <- sf::st_point(c(3, -1))
  edges <- sf::st_as_sf(sf::st_sfc(e2,
                                   sf::st_linestring(c(p2, p4, p3)),
                                   sf::st_linestring(c(p3, p5, p6, p3))))
  edges$from <- c(1, 1, 2)
  edges$to <- c(2, 2, 2)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_simplified <- simplify_network(network)
  nodes_simplified <- sf::st_geometry(sf::st_as_sf(network_simplified, "nodes"))
  edges_simplified <- sf::st_geometry(sf::st_as_sf(network_simplified, "edges"))
  edges_expected <- sf::st_sfc(e2)
  expect_setequal(nodes_simplified, nodes)
  expect_setequal(edges_simplified, edges_expected)
  # Also check that the simplification is run as part of the network cleaning
  network_clean <- clean_network(network)
  nodes_clean <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_clean <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  expect_setequal(nodes_clean, nodes_simplified)
  expect_setequal(edges_clean, edges_simplified)
})

test_that("Weights only include edge lengths if no opt args are given", {
  network_weights <- add_weights(network)
  edges <- sf::st_as_sf(network_weights, "edges")
  expect_true("weight" %in% colnames(edges))
  weight_actual <- edges[["weight"]]
  weight_expected <- sf::st_length(edges)
  expect_equal(weight_actual, weight_expected)
})

test_that("Weights can also include distance from target", {
  target <- sf::st_point(c(0, 0))
  network_weights <- add_weights(network, target = target)
  edges <- sf::st_as_sf(network_weights, "edges")
  expect_true("weight" %in% colnames(edges))
  weight_actual <- edges[["weight"]]
  weight_expected <- sf::st_length(edges) + drop(sf::st_distance(edges, target))
  expect_equal(weight_actual, weight_expected)
})

test_that("Weights can also include penalty in excluded area", {
  # Exclusion area is a circle around midpoint of e1, penalty should be added
  # only to e1
  center <- (p1 + p2) / 2
  area <- sf::st_buffer(center, 0.1)
  penalty <- 123
  network_weights <- add_weights(network, exclude_area = area,
                                 penalty = penalty)
  edges <- sf::st_as_sf(network_weights, "edges")
  expect_true("weight" %in% colnames(edges))
  weight_actual <- edges[["weight"]]
  weight_expected <- sf::st_length(edges)
  weight_expected[1] <- weight_expected[1] + penalty
  expect_equal(weight_actual, weight_expected)
})

test_that("Weights can account for both distance from target and excl. area", {
  target <- sf::st_point(c(0, 0))
  # Exclusion area is a circle around midpoint of e1, penalty should be added
  # only to e1
  center <- (p1 + p2) / 2
  area <- sf::st_buffer(center, 0.1)
  penalty <- 123
  network_weights <- add_weights(network, target = target,
                                 exclude_area = area, penalty = penalty)
  edges <- sf::st_as_sf(network_weights, "edges")
  expect_true("weight" %in% colnames(edges))
  weight_actual <- edges[["weight"]]
  weight_expected <- sf::st_length(edges) + drop(sf::st_distance(edges, target))
  weight_expected[1] <- weight_expected[1] + penalty
  expect_equal(weight_actual, weight_expected)
})

test_that("Weight name can be changed", {
  network_weights <- add_weights(network, weight_name = "length")
  edges <- sf::st_as_sf(network_weights, "edges")
  colnames_expected <- c("from", "to", "x", "length")
  expect_equal(colnames(edges), colnames_expected)
})

test_that("Shortest path works for single-edge path", {
  endpoints <- sf::st_sfc(p6, p7)
  path <- shortest_path(network_shortpath, from = endpoints[1],
                        to = endpoints[2], weights = "length")
  path_expected <- sf::st_sfc(sf::st_linestring(c(p6, p7)))
  expect_equal(path, path_expected)
})

test_that("Shortest path can reorient edges to return a LINESTRING", {
  # The expected path should merge edges "e8" and "e9", which have opposite
  # directions. The result should always be a linestring (not a
  # multi-linestring)
  endpoints <- sf::st_sfc(p6, p8)
  path <- shortest_path(network_shortpath, from = endpoints[1],
                        to = endpoints[2], weights = "length")
  path_expected <- sf::st_sfc(sf::st_linestring(c(p6, p10, p8)))
  expect_equal(path, path_expected)
})

test_that("Nearest node always return one point", {
  # Even if the feature is equidistant from two nodes
  target <- (p1 + p2) / 2
  nearest <- nearest_node(network, target)
  expect_length(nearest, 1)
  expected <- sf::st_sfc(p1)
  expect_equal(nearest, expected)
})

test_that("Nearest node also works with a linestring as target", {
  # Even if the feature is equidistant from two nodes
  target <- sf::st_linestring(c(sf::st_point(c(4, 0)),
                                sf::st_point(c(5, 0))))
  nearest <- nearest_node(network, target)
  expect_length(nearest, 1)
  expected <- sf::st_sfc(p3)
  expect_equal(nearest, expected)
})

test_that("Filter network properly splits network across adjacent regions", {
  area_1 <- sf::st_as_sfc(sf::st_bbox(c(xmin = -1, xmax = 0.5,
                                        ymin = -1, ymax = 1)))
  area_2 <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0.5, xmax = 1,
                                        ymin = -1, ymax = 1)))
  network_area_1 <- filter_network(network_shortpath, area_1)
  network_area_2 <- filter_network(network_shortpath, area_2)
  edges_area_1 <- sf::st_geometry(sf::st_as_sf(network_area_1, "edges"))
  nodes_area_1 <- sf::st_geometry(sf::st_as_sf(network_area_1, "nodes"))
  edges_area_2 <- sf::st_geometry(sf::st_as_sf(network_area_2, "edges"))
  nodes_area_2 <- sf::st_geometry(sf::st_as_sf(network_area_2, "nodes"))
  expect_length(edges_area_1, 2)
  expect_length(nodes_area_1, 3)
  expect_length(edges_area_2, 1)
  expect_length(nodes_area_2, 2)
})

test_that("Filter network drops smallest disconnected components", {
  # p4 is within the area, but it is left out since it remains disconnected
  # from the main network component
  area <- sf::st_as_sfc(sf::st_bbox(c(xmin = -1, xmax = 4,
                                      ymin = 0, ymax = 2)))
  network_filtered <- filter_network(network, area)
  edges_area <- sf::st_geometry(sf::st_as_sf(network_filtered, "edges"))
  nodes_area <- sf::st_geometry(sf::st_as_sf(network_filtered, "nodes"))
  expect_length(edges_area, 2)
  expect_length(nodes_area, 3)
})

test_that("Network setup with real data", {
  edges <- bucharest_osm$streets
  network <- as_network(edges, clean = FALSE, flatten = FALSE)
  edges_actual <- sf::st_geometry(sf::st_as_sf(network, "edges"))
  edges_expected <- sf::st_geometry(edges)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Flattening network with no crossings does not fail", {
  network_no_crossings_flat <- flatten_network(network_no_crossings)
  expect_true(inherits(network_no_crossings_flat, "sfnetwork"))
})
