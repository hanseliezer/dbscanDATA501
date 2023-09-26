classif <- read.csv("../classif.csv")

classif_str <- classif
classif_str$X20 <- c(rep("A", 200), rep("B", 300))
classif_logi <- classif
classif_logi$X21 <- c(rep(TRUE, 200), rep(FALSE, 100), rep(TRUE, 200))

classif_distmat <- as.matrix(dist(classif))
wrong_distmat <- matrix(rnorm(100), nrow=100, ncol=100)
nonsense_distmat <- matrix(rnorm(100), nrow=100, ncol=100)
diag(nonsense_distmat) <- 0

test_min_pts <- 4
test_eps <- 5

test_that("Verify data argument", {
  
  # can't be NULL
  expect_error(dbscan(NULL, test_eps, test_min_pts),
               "Please supply a dataset/distance matrix.")
  
  # can't be empty
  expect_error(dbscan(classif['X1' < -1000], test_eps, test_min_pts),
               "Please supply a dataset/distance matrix")
  
  # data must be a matrix/dataframe
  expect_error(dbscan(9, test_eps, test_min_pts),
               "data must be a matrix or data.frame.")
  
  # data types must be all numeric
  expect_error(dbscan(classif_str, test_eps, test_min_pts),
               "data must consist entirely of numeric type.")
  
  # data types must be all numeric
  expect_error(dbscan(classif_logi, test_eps, test_min_pts),
               "data must consist entirely of numeric type.")
  
  # data is of correct type, columns are of correct type
  expect_silent(dbscan(classif, test_eps, test_min_pts))
  
})

test_that("Verify min_pts and eps arguments", {
  
  # can't be strings
  expect_error(dbscan(classif, "test_eps", "test_min_pts"),
               "Both min_pts and eps must be integers equal or bigger than 0.")
  
  # can't be logical
  expect_error(dbscan(classif, FALSE, TRUE),
               "Both min_pts and eps must be integers equal or bigger than 0.")
  
  # can't be a dataframe
  expect_error(dbscan(classif, classif, test_min_pts),
               "Both min_pts and eps must be integers equal or bigger than 0.")
  
  # both are numbers
  expect_silent(dbscan(classif, test_eps, test_min_pts))
  
  # both are numbers
  expect_silent(dbscan(classif, 3L, 4L))
  
  # both are numbers
  expect_silent(dbscan(classif, 0, 0))
  
})

test_that("Verify metric argument", {
  
  # unavailable distace metric
  expect_error(dbscan(classif, test_eps, test_min_pts, metric='chebyshev'),
               "Options for distance metric are 'euclidean', 'manhattan' or 'precomputed'.")
  
  # available metric
  expect_silent(dbscan(classif, test_eps, test_min_pts, metric='euclidean'))
  
  # available metric
  expect_silent(dbscan(classif, test_eps, test_min_pts, metric='manhattan'))
  
})

test_that("Verify shape of data/distance matrix", {

  # if distance is precomputed, data must already be a distance matrix
  expect_error(dbscan(classif, test_eps, test_min_pts, metric='precomputed'),
               "Distance matrix must be of size n x n.")
  
  # distance matrix must have all diagonal elements be 0
  expect_error(dbscan(wrong_distmat, test_eps, test_min_pts, metric='precomputed'),
               "Distance matrix must have all diagonal elements be 0.")

  # correct distance matrix: n x n and all diagonal elements are 0
  expect_silent(dbscan(nonsense_distmat, test_eps, test_min_pts, metric='precomputed'))

})

test_that("Verify normalise and border_pts arguments", {

  # normalise must be logical
  expect_error(dbscan(classif, test_eps, test_min_pts, normalise=9),
               "Both normalise and borderPoints must be of type logical.")
  
  # border_pts must be logical
  expect_error(dbscan(classif, test_eps, test_min_pts, border_pts="peep"),
               "Both normalise and borderPoints must be of type logical.")

  # correctly set to logical
  expect_silent(dbscan(classif, test_eps, test_min_pts, normalise=FALSE, border_pts=FALSE))

})