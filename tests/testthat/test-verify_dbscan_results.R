data(circles)

test_min_pts <- 4
test_eps <- 5

# use fpc and dbscan's dbscan as the reference, consider them to be the "correct" answers
# fpc and dbscan need their data to be scaled beforehand
dbscan_true_fpc <- fpc::dbscan(scale(circles), test_min_pts, test_eps)$cluster
dbscan_true_dbs <- dbscan::dbscan(scale(circles), test_min_pts, test_eps)$cluster
dbscan_my <- dbscan(circles, test_min_pts, test_eps)$cluster_labs

corepts_true_dbs <- dbscan::is.corepoint(scale(circles), test_min_pts, test_eps)
corepts_my <- is_core_point(circles, test_min_pts, test_eps)

test_that("Verify clustering results: default", {
  
  # compare cluster labels with fpc
  expect_equal(dbscan_true_fpc, dbscan_my)
  
  # compare cluster labels with dbscan
  expect_equal(dbscan_true_dbs, dbscan_my)
  
  # compare list of core points
  expect_equal(corepts_true_dbs, corepts_my)
  
})

test_min_pts_2 <- 6
test_eps_2 <- 0.055

# test clustering using a distance matrix and a different distance metric
circles_dist <- dist(circles, method="minkowski")
circles_distmat <- as.matrix(circles_dist)

dbscan_true_fpc_dist <- fpc::dbscan(circles_dist, test_min_pts_2, test_eps_2, method="dist")$cluster
dbscan_true_dbs_dist <- dbscan::dbscan(circles_dist, test_eps_2, test_min_pts_2)$cluster
dbscan_my_dist <- dbscan(circles_distmat, test_eps_2, test_min_pts_2, metric='precomputed')$cluster_labs

corepts_true_dbs_dist <- dbscan::is.corepoint(circles_dist, test_min_pts_2, test_eps_2)
corepts_my_dist <- is_core_point(circles_distmat, test_min_pts_2, test_eps_2, metric='precomputed')

test_that("Verify clustering results: using distance matrix", {
  
  expect_equal(dbscan_true_fpc_dist, dbscan_my)
  
  expect_equal(dbscan_true_dbs_dist, dbscan_my_dist)
  
  expect_equal(corepts_true_dbs_dist, corepts_my_dist)
  
})

