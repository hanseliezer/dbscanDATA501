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
  
  # checking min_pts and eps: both must be numeric and > 0. tests for these two now
  # split up to make the errors clearer
  # this first if would capture cases when eps are not scalars (is.numeric alone is
  # not enough as matrices are considered TRUE)
  if (!(is.numeric(eps) && length(eps) == 1)) {
    stop("eps must be integers equal or bigger than 0.")
  }
  # second case is if eps is indeed scalar but less than 0
  else if (is.numeric(eps) && eps < 0) {
    stop("eps must be integers equal or bigger than 0.")
  }
  
  if (!(is.numeric(min_pts) && length(min_pts) == 1)) {
    stop("min_pts must be integers equal or bigger than 0.")
  }
  else if (is.numeric(min_pts) && min_pts < 0) {
    stop("min_pts must be integers equal or bigger than 0.")
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
  
  # checking normalise and border_pts: should be boolean
  if (!is.logical(normalise)) {
    stop("normalise must be TRUE/FALSE.")
  }
  if (!is.logical(border_pts)) {
    stop("border_pts must be TRUE/FALSE.")
  }
}

# constructor function of a `dbscan` class
def_dbscan <- function(data, eps, min_pts, metric, normalise, border_pts, cluster_labs, duration) {
  dbscan_list <- list("dataset"=data,
                      "eps"=eps,
                      "min_pts"=min_pts,
                      "metric"=metric,
                      "normalise"=normalise,
                      "border_pts"=border_pts,
                      "cluster_labs"=cluster_labs,
                      "fitting_time"=duration
                      )
  
  dbscan_obj <- structure(dbscan_list,
                          class="dbscan")
  return(dbscan_obj)
}