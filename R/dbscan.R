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
#' @returns `dbscan()` returns a list object of class `dbscan` with the following components:
#' 
#' \item{dataset }{ Original dataset being clustered.}
#' \item{eps }{ Value of `eps` parameter used.}
#' \item{min_pts }{ Value of `min_pts` parameter used.}
#' \item{metric }{ Distance metric used.}
#' \item{border_pts }{ Whether border points are clustered.}
#' \item{cluster_labs }{ Integer vector with indicating cluster membership. Noise points are assigned 0.}
#' \item{fitting_time }{ Total algorithm fitting time in seconds.}
#' 
#' `is_core_point()` returns a logical vector indicating whether each observation in the dataset would be
#' considered a core point under the given `eps` and `min_pts` parameters.
#' 
#' @details
#' The most critical parameters to set are `min_pts` and `eps`.
#' 
#' If dataset is not yet normalised, it is highly recommended to set `normalise` to true, or normalise
#' the data first separately. As a distance-based algorithm, DBSCAN is sensitive to features being on
#' varying scales.
#' 
#' `border_pts=TRUE` means border points - points that do not have sufficient number of neighbours by itself
#' but are neighbours of a core point - are included in a cluster, and is consistent to the original algorithm
#' as described by Ester et al. (1996). While `FALSE` excludes border points, which is more consistent to the
#' definition of a cluster having to always have consistent minimum density and is equivalent to hierarchical
#' DBSCAN described in Campello et al. (2013).
#' 
#' @keywords clustering
#' @references
#' Ester, Martin, Hans-Peter Kriegel, Jörg Sander, and Xiaowei Xu. 1996. “A Density-Based Algorithm for
#' Discovering Clusters in Large Spatial Databases with Noise.” In *KDD ’96: Proceedings of the 2nd
#' International Conference on Knowledge Discovery and Data Mining*, 226–31. Palo Alto, CA, United States:
#' AAAI Press.
#' 
#' Campello, Ricardo J. G. B., Davoud Moulavi and Jörg Sander. 2013. "Density-Based Clustering Based on
#' Hierarchical Density Estimates." In *PAKDD 2013: Proceedings of the 17th Pacific-Asia Conference on
#' Knowledge Discovery in Databases, 160-172*. Berlin, Germany: Springer.
#' @examples
#' \dontrun{
#' blobs <- read.csv('blobs.csv')
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