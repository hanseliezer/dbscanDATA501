#' Demonstration of DBSCAN on toy datasets
#' 
#' @description
#' Applies DBSCAN to 2-D toy datasets and displays the results. `blobs` represents points collected into two
#' clearly-separated approximately-globular clusters, and `circles` represents points collected into two hollow 
#' circles inside each other. Three different algorithms will be fitted into the dataset with different `min_pts`
#' and `eps` parameters, and scatter plots drawn to display the results to demonstrate the impact of the different
#' parameters.
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' blobs()
#' circles()
#' }
#' @rdname demo.dbscan
#' @export
blobs <- function() {
  print("lorem ipsum")
}

#' @rdname demo.dbscan
#' @export
circles <- function() {
  print("lorem ipsum 2")
}