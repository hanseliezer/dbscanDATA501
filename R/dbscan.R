#' Density-based spatial clustering of applications with noise (DBSCAN)
#' 
#' @description
#' Applies the DBSCAN algorithm to a given dataset.
#' 
#' @param data Dataset or distance matrix to be clustered.
#' @param eps Maximum distance in which to find `min_pts` neighbours to form a cluster.
#' @param min_pts Minimum number of points required to be found within `eps` distance to form a cluster.
#' @param metric Distance metric. Defaults to `euclidean`, other options are `manhattan` and
#' `precomputed`. If `precomputed`, `data` must be a distance matrix.
#' @param normalise Logical; whether to normalise `data` before fitting. Defaults to `TRUE`.
#' @param border_pts Logical; whether "border points" should be included in a cluster.
#' Defaults to `TRUE`.
#' 
#' @returns `dbscan()` returns a list object of class `dbscanDATA501` with the following components:
#' 
#' \item{dataset }{Dataset being clustered.}
#' \item{eps }{Value of `eps` parameter used.}
#' \item{min_pts }{Value of `min_pts` parameter used.}
#' \item{metric }{Distance metric used.}
#' \item{normalise }{Whether dataset is normalised prior to fitting.}
#' \item{border_pts }{Whether border points are clustered.}
#' \item{cluster_labs }{Integer vector indicating cluster membership. Noise points are assigned 0.}
#' \item{fitting_time }{Total algorithm fitting time in seconds.}
#' 
#' `is_core_point()` returns a logical vector indicating whether each observation in the dataset would be
#' considered a core point under the given `eps` and `min_pts` parameters.
#' 
#' @details
#' The most critical parameters to choose are `min_pts` and `eps`. The higher the numbers, the higher the
#' bar is for a cluster to be initiated. The best values will be dependent on the dataset itself
#' and domain knowledge, and it is recommended to try a few different combinations of values before
#' proceeding with analysis. A proposed heuristic to get an initial `min_pts` guess is dataset
#' dimensionality multiplied by 2, while a starting value for `eps` can be found by constructing a sorted
#' \eqn{k}-distance plot based on a chosen `min_pts` and finding the "valley" (Sander 1998).
#' 
#' The choice of `metric` is also problem-dependent. If neither Euclidean and Manhattan distances are
#' appropriate, a custom distance matrix can always be calculated before supplying it to the function
#' and specifying `metric = precomputed`.
#' 
#' If dataset is not yet normalised, it is highly recommended to set `normalise = TRUE`, or normalise
#' the data first separately. As a distance-based algorithm, DBSCAN is sensitive to features being on
#' varying scales.
#' 
#' `border_pts=TRUE` means border points - points that do not have sufficient number of neighbours by
#' itself but are neighbours of a core point - are included in a cluster, and is consistent to the
#' original algorithm as described by Ester et al. (1996). While `FALSE` excludes border points, which
#' is more consistent to the definition of a cluster having to always have a certain minimum density,
#' and is equivalent to hierarchical DBSCAN described in Campello et al. (2013).
#' 
#' @keywords clustering
#' @references
#' Sander, J., Ester, M., Kriegel, HP. et al. 1998. "Density-Based Clustering in Spatial Databases: The
#' Algorithm GDBSCAN and Its Applications." *Data Mining and Knowledge Discovery* 2: 169–194.
#' https://doi.org/10.1023/A:1009745219419
#' 
#' Ester, Martin, Hans-Peter Kriegel, Jörg Sander, and Xiaowei Xu. 1996. “A Density-Based Algorithm for
#' Discovering Clusters in Large Spatial Databases with Noise.” In *KDD ’96: Proceedings of the 2nd
#' International Conference on Knowledge Discovery and Data Mining*, pp. 226–31. Palo Alto, CA, United
#' States: AAAI Press.
#' 
#' Campello, Ricardo J. G. B., Davoud Moulavi and Jörg Sander. 2013. "Density-Based Clustering Based on
#' Hierarchical Density Estimates." In *PAKDD 2013: Proceedings of the 17th Pacific-Asia Conference on
#' Knowledge Discovery in Databases*, pp. 160-172. Berlin, Germany: Springer.
#' 
#' @examples
#' \dontrun{
#' data(blobs)
#' dbscan(blobs, 5, 0.2)
#' }
#' @rdname dbscan
#' @export
dbscan <- function(data, eps, min_pts, metric="euclidean", normalise=TRUE, border_pts=TRUE) {
  # input validation
  dbscan_input_checks(data, eps, min_pts, metric, normalise, border_pts)
  
  # if precomputed, data is already a distance matrix
  if (metric == 'precomputed') {
    dist_mat <- data
  }
  else {
    # normalise data if normalise=TRUE, else use given data
    data_scl <- if (normalise) scale(data) else data
    # calculate distance matrix based on scaled data
    dist_mat <- as.matrix(dist(data_scl, method=metric))
  }
  
  # regardless of implementation (brute or tree), list of core points should be
  # the same
  is_core_pt <- apply(dist_mat, 1, function(x) {
    sum(x > 0 & x <= eps) + 1 >= min_pts
  })
  core_pts <- as.vector(which(is_core_pt))
  # anything that is not a core point is simply a non-core point
  noncore_pts <- as.vector(which(!is_core_pt))
  
  n_obs <- nrow(dist_mat)
  clust_lab <- rep(NA, n_obs)
  clust_num <- 1
  
  # start time of algorithm fitting
  start <- Sys.time()
  # generate initial clusters consisting only of core points (points that fulfill the requirement
  # of having min_pts points within eps radius)
  for (o in 1:n_obs) {
    if (is.na(clust_lab[o])) {
      neighbours <- find_neighbours(dist_mat[o, ], eps)
      if (length(neighbours) + 1 >= min_pts) {
        clust_lab[o] <- clust_num
        clust_lab <- create_init_clusters(dist_mat, eps, min_pts, neighbours, clust_lab, clust_num)
        clust_num <- clust_num + 1
      }
    }
  }
  
  # if border points are included, then expand clusters by looking at each non-core point to find
  # if they are a neighbour of a core point. if border points are not included, then clusters consist
  # entirely only of core points
  if (border_pts) {
    clust_lab <- expand_clusters(dist_mat, eps, core_pts, noncore_pts, clust_lab)
  }
  
  # finish time of algorithm fitting
  finish <- Sys.time()
  # Sys.time() returns a POSIXct class object, so convert it to keep only the
  # value
  duration <- as.numeric(finish - start)
  # any labels that are still NA will be noise, "labelled" as 0
  clust_lab <- replace(clust_lab, is.na(clust_lab), 0)
  rm(dist_mat)
  
  result <- def_dbscan(data, eps, min_pts, metric, normalise, border_pts, clust_lab, duration)
  return(result)
}

#' @rdname dbscan
#' @export
is_core_point <- function(data, eps, min_pts, metric="euclidean", normalise=TRUE) {
  # not the actual algorithm, so no need to do deliberate input checking (?)
  if (metric == 'precomputed') {
    dist_mat <- data
  }
  else {
    data_scl <- if (normalise) scale(data) else data
    # calculate distance matrix based on scaled data
    dist_mat <- as.matrix(dist(data_scl, method=metric))
  }
  
  is_core_pt <- apply(dist_mat, 1, function(x) {
    sum(x > 0 & x <= eps) + 1 >= min_pts
  })
  
  # un-name the logical vector
  return(unname(is_core_pt))
}

##############################################################################

# helper functions
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
  # checking data: cannot contain any NAs/NaNs
  else if (any(is.na(data))) {
    stop("data must not contain any 'NA's/'NaN's.")
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
                          class="dbscanDATA501")
  return(dbscan_obj)
}