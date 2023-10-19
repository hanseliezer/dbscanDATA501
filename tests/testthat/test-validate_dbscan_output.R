data(blobs)
test_min_pts <- 4
test_eps <- 5

blobs_dbscan <- dbscan(blobs, test_eps, test_min_pts)

test_that("Verify output and it's contents", {
  
  # output is of class 'dbscan'
  expect_equal(class(blobs_dbscan), "dbscanDATA501")
  
  # output should be a list of length 8
  expect_equal(length(blobs_dbscan), 8)
  
  # saved parameters should be equal to what was initially set
  expect_equal(blobs_dbscan$eps, test_eps)
  
  expect_equal(blobs_dbscan$min_pts, test_min_pts)
  
  # length of label vector should equal to the number of observations in the dataset
  expect_equal(length(blobs_dbscan$cluster_labs), nrow(blobs))
  
})