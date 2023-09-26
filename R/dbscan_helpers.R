# input validation
dbscan_input_checks <- function(data, eps, min_pts, metric, normalise, border_pts) {
  # checking data: can't be null or an empty table
  if (is.null(data) | length(data) == 0) {
    stop("Please supply a dataset/distance matrix.")
  }
  # checking data: must be a matrix or data.frame
  else if (!is.matrix(data) & !is.data.frame(data)) {
    stop("data must be a matrix or data.frame.")
  }
  # checking data: must be all numeric
  else if (!all(sapply(data, is.numeric))) {
    stop("data must consist entirely of numeric type.")
  }
  
  # checking min_pts and eps: both must be numeric and > 0
  if (!all(sapply(c(eps, min_pts), is.numeric)) | any(eps < 0, min_pts < 0)) {
    stop("Both min_pts and eps must be integers equal or bigger than 0.")
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

  if (!all(sapply(c(normalise, border_pts), is.logical))) {
    stop("Both normalise and borderPoints must be of type logical.")
  }
}

# constructor function of a `dbscan` class
def_dbscan <- function(data, eps, min_pts, metric, border_pts, cluster_labs, duration) {
  dbscan_list <- list("dataset"=data,
                      "eps"=eps,
                      "min_pts"=min_pts,
                      "metric"=metric,
                      "border_pts"=border_pts,
                      "cluster_labs"=cluster_labs,
                      "fitting_time"=duration
                      )
  
  dbscan_obj <- structure(dbscan_list,
                          class="dbscan")
  return(dbscan_obj)
}