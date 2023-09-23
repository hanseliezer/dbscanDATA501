# input validation
dbscan_input_checks <- function(data, eps, min_pts, metric, normalise, borderPoints) {
  # checking data: can't be null
  if (is.null(data)) {
    stop("Please supply a dataset/distance matrix")
  }
  # checking data: must be a matrix or data.frame
  else if (!is.matrix(data) & !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  
  # check distance metric: must be one of the three options
  if (!metric %in% c("euclidean", "manhattan", "precomputed")) {
    stop("Options for distance metric are 'euclidean', 'manhattan' or 'precomputed'.")
  }
  # check distance metric: if precomputed, then data must be a distance matrix
  if (metric == 'precomputed') {
    if (nrow(data) != ncol(data)) {
      stop("Distance matrix must be of size n x n.")
    }
    else if (sum(diag(data)) != 0) {
      stop("Distance matrix must have all diagonal elements be 0.")
    }
  }
  
  # checking min_pts and eps: both must be numeric and > 0
  if (!all(sapply(c(eps, min_pts), is.numeric)) | any(eps <= 0, min_pts <= 0)) {
    stop("Both min_pts and eps must be non-zero positive numbers.")
  }
  
  if (!all(sapply(c(normalise, borderPoints), is.logical))) {
    stop("Both normalise and borderPoints must be of type logical.")
  }
}

# constructor function of a `dbscan` class
def_dbscan <- function(data, eps, minpts, labs, dura, metr) {
  dbscan_list <- list("dataset"=data,
                      "eps"=eps,
                      "min_pts"=minpts,
                      "metric"=metr,
                      "cluster_labels"=labs,
                      "fitting_time"=dura)
  
  dbscan_obj <- structure(dbscan_list,
                          class="dbscan")
  return(dbscan_obj)
}