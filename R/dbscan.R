#' Density-based spatial clustering of applications with noise (DBSCAN)
#' 
#' @description
#' Applies the DBSCAN algorithm to a given dataset.
#' 
#' @param data Dataset or distance matrix to be clustered.
#' @param min_pts Minimum number of points required to be found within \code{eps} distance to form a cluster.
#' @param eps Maximum distance in which to find \code{min_pts} neighbours to form a cluster.
#' @param metric Distance metric. Defaults to \code{euclidean}, other options are \code{manhattan} and
#' \code{precomputed}. If \code{precomputed}, \code{data} must be a distance matrix.
#' @param normalise Logical; whether to normalise \code{data} before fitting. Defaults to \code{TRUE}.
#' @param borderPoints Logical; whether "border points" should be included in a cluster.
#' Defaults to \code{FALSE}.
#' 
#' @details
#' border points equal \code{FALSE} is consistent with DBSCAN* described in Campello et al. 2013
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' blobs <- read.csv('blobs.csv')
#' dbscan(blobs, 5, 0.2)
#' }
#' @export

dbscan <- function(data, min_pts, eps, metric="euclidean", normalise=TRUE, borderPoints=FALSE) {
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
  if (!all(sapply(c(min_pts, eps), is.numeric)) | any(min_pts <= 0, eps <= 0)) {
    stop("Both min_pts and eps must be non-zero positive numbers.")
  }
  
  if (!all(sapply(c(normalise, borderPoints), is.logical))) {
    stop("Both normalise and borderPoints must be boolean.")
  }
  
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
  
  n_obs <- nrow(dist_mat)
  clust_lab <- rep(NA, n_obs)
  clust_num <- 1
  
  # start time of algorithm fitting
  start <- Sys.time()
  # if border points are included, expand clusters starting from core points;
  # if not, expand starting from the first row of distance matrix
  loop <- if (borderPoints) core_pts else c(1:n_obs)
  for (o in loop) {
    if (is.na(clust_lab[o])) {
      neighbours <- find_neighbours(dist_mat[o, ], eps)
      if (length(neighbours) + 1 >= 5) {
        if (is.na(clust_lab[o])) {
          clust_lab[o] <- clust_num
        }
        clust_lab <- expand_clusters(dist_mat, min_pts, eps, borderPoints,
                                     neighbours, clust_lab, clust_num)
        clust_num <- clust_num + 1
      }
    }
  }
  # finish time of algorithm fitting
  finish <- Sys.time()
  # Sys.time() returns a POSIXct class object, so convert it to keep only the
  # value
  duration <- as.numeric(finish - start)
  # any labels that are still NA will be noise, "labelled" as 0
  clust_lab <- replace(clust_lab, is.na(clust_lab), 0)
  rm(dist_mat)
  
  result <- def_dbscan(data, clust_lab, duration, metric)
  return(result)
}