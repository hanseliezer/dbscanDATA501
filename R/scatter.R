#' Scatter plot of DBSCAN clusters.
#' 
#' @description
#' Displays clustered dataset on a 2-D scatter plot with the DBSCAN-generated clusters as the grouping variable.
#' 
#' @param obj Object of class `dbscan`; will contain the full dataset and cluster labels.
#' @param x Feature to be displayed on x-axis.
#' @param y Feature to be displayed on y-axis.
#' @param how How generated clusters are displayed as the grouping of data points. Defaults to `colour` (i.e.
#' displays the clusters with different colours); other options are `shape` and `both` (colour *and* shape).
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' blobs <- read.csv('blobs.csv')
#' dbs <- dbscan(blobs, 5, 0.2)
#' scatter(dbs, x=X, y=Y, how='colour')
#' }
#' @export
scatter.dbscan <- function(obj, x, y, how='colour') {
  print("function under construction")
}