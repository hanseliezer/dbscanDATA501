data(circles)

test_min_pts <- 0.1
test_eps <- 6

circles_dbscan <- dbscan(circles, test_eps, test_min_pts)

test_that("Verify arguments", {
  
  # ax1 must be numeric or character
  expect_error(plot(circles_dbscan, ax1=TRUE, ax2=2),
               "ax1 must be an index referring to a column, or a column name.")
  
  # ax1 must be numeric or character
  expect_error(plot(circles_dbscan, ax1=circles, ax2=2),
               "ax1 must be an index referring to a column, or a column name.")
  
  # if ax1 is numeric, then it must be an index that is within the range of the number of columns in
  # the dataset
  expect_error(plot(circles_dbscan, ax1=3, ax2=2),
               # this is a regex expression, needs \\( for it to interpret parentheses as literal characters
               "If ax1 is an index referring to a column, it must be 1 <= ax1 <= \\(number of dataset columns\\).")
  
  # if ax1 is character, then it must refer to a column name that is on the dataset
  expect_error(plot(circles_dbscan, ax1="Z", ax2=2),
               "If ax1 is a column name, it must exist in dataset.")
  
  # same tests but for ax2
  expect_error(plot(circles_dbscan, ax1=2, ax2=FALSE),
               "ax2 must be an index referring to a column, or a column name.")

  expect_error(plot(circles_dbscan, ax1=1, ax2=circles),
               "ax2 must be an index referring to a column, or a column name.")

  expect_error(plot(circles_dbscan, ax1=1, ax2=99),
               "If ax2 is an index referring to a column, it must be 1 <= ax2 <= \\(number of dataset columns\\).")
  
  # ax1 is checked first, so if both ax1 and ax2 are wrong, the error is for ax1
  expect_error(plot(circles_dbscan, ax1="XXX", ax2="XD"),
               "If ax1 is a column name, it must exist in dataset.")
  
  # kind must be one of 'colour', 'shape' or 'both'
  expect_error(plot(circles_dbscan, kind="english"),
               "Options for 'kind' are one of 'colour', 'shape', and 'both'.")
  # kind must be one word
  expect_error(plot(circles_dbscan, kind=c("colour", "shape")),
               "Please select one of 'colour', 'shape', and 'both'.")
  expect_error(plot(circles_dbscan, kind=circles),
               "Please select one of 'colour', 'shape', and 'both'.")
  
  # supplying additional y/type/col/pch will be ignored
  expect_warning(plot(circles_dbscan, type="l"),
                 "Supplied additional arguments contains one of 'y', 'type', 'col', and 'pch', which has been assigned. These will be ignored.")
  expect_warning(plot(circles_dbscan, kind="shape", col="gray", pch=13, main="Test plot"),
                 "Supplied additional arguments contains one of 'y', 'type', 'col', and 'pch', which has been assigned. These will be ignored.")
  
  expect_silent(plot(circles_dbscan, ax1=2, ax2=1))
  expect_silent(plot(circles_dbscan, ax1=1, ax2=2, kind="shape"))
  expect_silent(plot(circles_dbscan, kind="both", main="Another test plot", xlab="Side 1", ylab="Side 2"))
  
})
