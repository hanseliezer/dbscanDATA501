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

test_that("Verify clustering results", {
  
  # compare cluster labels with fpc
  expect_equal(dbscan_true_fpc, dbscan_my)
  
  # compare cluster labels with dbscan
  expect_equal(dbscan_true_dbs, dbscan_my)
  
  # compare list of core points
  expect_equal(corepts_true_dbs, corepts_my)
  
})