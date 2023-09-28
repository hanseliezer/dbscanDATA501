classif <- read.csv("../../dat/classif.csv")
test_min_pts <- 4
test_eps <- 5

classif_dbscan <- dbscan(classif, test_eps, test_min_pts)

test_that("Verify output and it's contents", {
  
  # output is of class 'dbscan'
  expect_equal(class(classif_dbscan), "dbscan")
  
  # output should be a list of length 7
  expect_equal(length(classif_dbscan), 7)
  
  # saved parameters should be equal to what was initially set
  expect_equal(classif_dbscan$eps, test_eps)
  
  expect_equal(classif_dbscan$min_pts, test_min_pts)
  
  # length of label vector should equal to the number of observations in the dataset
  expect_equal(length(classif_dbscan$cluster_labs), nrow(classif))
  
})