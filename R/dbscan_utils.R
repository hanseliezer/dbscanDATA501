# function returning a vector of observation indices which are neighbours of a point
find_neighbours <- function(dist_vec, eps) {
  neigh_pts <- which(dist_vec > 0 & dist_vec <= eps)
  return(as.vector(neigh_pts))
}

# function to expand clusters, which amounts to recursively finding neighbours of a
# point, then finding the neighbours of those neighbours, etc.
expand_clusters <- function(dist_mat, min_pts, eps, borderPoints, neigh_pts, labs, clust_num) {
  # if border points are included, expand regardless if neighbour's neighbour
  # has enough points or not
  if (borderPoints) {
    for (n in neigh_pts) {
      if (is.na(labs[n])) {
        neighbours_neighbour <- find_neighbours(dist_mat[n, ], eps)
        labs[n] <- clust_num
        labs <- expand_clusters(dist_mat, min_pts, eps,borderPoints,
                                neighbours_neighbour, labs, clust_num)
      }
    }
  }
  # if border points are not included, only expand if neighbour's neighbour
  # has enough points
  else {
    for (n in neigh_pts) {
      if (is.na(labs[n])) {
        neighbours_neighbour <- find_neighbours(dist_mat[n, ], eps)
        if (length(neighbours_neighbour) + 1 >= min_pts) {
          labs[n] <- clust_num
          labs <- expand_clusters(dist_mat, min_pts, eps,borderPoints,
                                  neighbours_neighbour, labs, clust_num)
        }
      }
    }
  }
  return(labs)
}

# constructor function of a `dbscan` class
def_dbscan <- function(data, labs, dura, metr) {
  dbscan_list <- list("dataset"=data,
                      "cluster_labels"=labs,
                      "fitting_time"=dura,
                      "metric"=metr)
  dbscan_obj <- structure(dbscan_list,
                          class="dbscan")
  return(dbscan_obj)
}