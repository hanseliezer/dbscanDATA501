#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_neighbours(NumericVector dist_vec, double eps) {
  IntegerVector neigh_pts = {Rcpp::Range(1, dist_vec.length())};
  neigh_pts = neigh_pts[(dist_vec > 0) & (dist_vec <= eps)];
  return neigh_pts;
}

// [[Rcpp::export]]
IntegerVector create_init_clusters(NumericMatrix dist_mat,
                                   double eps,
                                   int min_pts,
                                   IntegerVector neigh_pts,
                                   IntegerVector labs,
                                   int clust_num) {
  
  // generate initial clusters, only consisting of core points (points that fulfill the requirement
  // of having min_pts points within eps radius)
  for (int n=0; n < neigh_pts.length(); n++) {
    int idx = neigh_pts[n] - 1;
    if (IntegerVector::is_na(labs[idx])) {
      IntegerVector neighbours_neighbour = find_neighbours(dist_mat(idx, _), eps);
      if (neighbours_neighbour.length() + 1 >= min_pts) {
        labs[idx] = clust_num;
        labs = create_init_clusters(dist_mat, eps, min_pts, neighbours_neighbour, labs, clust_num);
      }
    }
  }
  return labs;
}

// [[Rcpp::export]]
IntegerVector expand_clusters(NumericMatrix dist_mat,
                              double eps,
                              IntegerVector core_pts,
                              IntegerVector noncore_pts,
                              IntegerVector labs) {
  
  // expand clusters by adding non-core/border points: if a non-core point is a neighbour of a core
  // point, then the non-core point should belong in the same cluster
  for (int nc=0; nc < noncore_pts.length(); nc++) {
    int idx_nc = noncore_pts[nc] - 1;
    if (IntegerVector::is_na(labs[idx_nc])) {
      IntegerVector neighbours = find_neighbours(dist_mat(idx_nc, _), eps);
      for (int n=0; n < neighbours.length(); n++) {
        if (std::find(core_pts.begin(), core_pts.end(), neighbours[n]) != core_pts.end()) {
          int idx_n = neighbours[n] - 1;
          labs[idx_nc] = labs[idx_n];
          break;
        }
      }
    }
  }
  return labs;
}