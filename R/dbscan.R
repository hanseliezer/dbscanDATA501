#' Density-based spatial clustering of applications with noise (DBSCAN)
#' 
#' @description
#' Applies the DBSCAN algorithm to a given dataset.
#' 
#' @param data Dataset or distance matrix to be clustered.
#' @param eps Maximum distance in which to find \code{min_pts} neighbours to form a cluster.
#' @param min_pts Minimum number of points required to be found within \code{eps} distance to form a cluster.
#' @param metric Distance metric. Defaults to \code{euclidean}, other options are \code{manhattan} and
#' \code{precomputed}. If \code{precomputed}, \code{data} must be a distance matrix.
#' @param normalise Logical; whether to normalise \code{data} before fitting. Defaults to \code{TRUE}.
#' @param borderPoints Logical; whether "border points" should be included in a cluster.
#' Defaults to \code{FALSE}.
#' 
#' @details
#' \code{borderPoints = TRUE} would be equivalent to the original DBSCAN algorithm described by Ester et al.,
#' while \code{FALSE} would be equivalent to DBSCAN* described in Campello et al. 2013
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' blobs <- read.csv('blobs.csv')
#' dbscan(blobs, 5, 0.2)
#' }
#' @export

dbscan <- function(data, eps, min_pts, metric="euclidean", normalise=TRUE, borderPoints=FALSE) {
  # input validation
  dbscan_input_checks(data, eps, min_pts, metric, normalise, borderPoints)
  
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
  if (borderPoints) {
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
  
  result <- def_dbscan(data, eps, min_pts, clust_lab, duration, metric)
  return(result)
}