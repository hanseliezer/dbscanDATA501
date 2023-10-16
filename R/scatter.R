#' Scatter plot of DBSCAN clusters.
#' 
#' @description
#' Displays clustered dataset on a 2-D scatter plot with the DBSCAN-generated clusters as the grouping variable.
#' 
#' @param obj Object of class `dbscan`; will contain the full dataset and cluster labels.
#' @param x Feature to be displayed on x-axis. Either column index or name.
#' @param y Feature to be displayed on y-axis. Either column index or name.
#' @param how How clusters are displayed as the grouping of data points. Default is `colour` (i.e.
#' displays the clusters with different colours); other options are `shape` and `both` (colour *and* shape).
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' blobs <- read.csv('blobs.csv')
#' dbs <- dbscan(blobs, 5, 0.2)
#' scatter_plot(dbs, x=X, y=Y, how='colour')
#' }
#' @export
scatter_plot <- function(obj, x=1, y=2, kind='colour') {
  # input validation
  scatter_input_checks(obj, x, y, kind)
  
  # make axis labels
  x_label <- if (is.numeric(x)) colnames(obj$dataset)[x] else x
  y_label <- if (is.numeric(y)) colnames(obj$dataset)[y] else y
  
  # replace 0 labels with 'Noise'
  classes <- replace(obj$cluster_labs, obj$cluster_labs == 0, "Noise")
  classes <- factor(classes)
  classes_legend <- levels(classes)
  
  # xpd allows stuff to be created outside the initial plot
  par(mar=c(5.1, 4, 4, 5.25), xpd=TRUE)
  
  if (kind == 'colour') {
    plot(obj$dataset[, x], obj$dataset[, y],
         col=classes, pch=1,
         xlab=x_label, ylab=y_label, main="Scatter plot of DBSCAN clusters")
    # legend location is top left, but inset shifts it 101% to the right so now it's
    # placed top right just outside the plot
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col=factor(classes_legend), pch=1)
  }
  
  else if (kind == 'shape') {
    plot(obj$dataset[, x], obj$dataset[, y],
         col="red", pch=as.numeric(classes),
         xlab=x_label, ylab=y_label, main="Scatter plot of DBSCAN clusters")
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col="red", pch=factor(classes_legend))
  }
  
  else if (kind == 'both') {
    plot(obj$dataset[, x], obj$dataset[, y],
         col=classes, pch=as.numeric(classes),
         xlab=x_label, ylab=y_label, main="Scatter plot of DBSCAN clusters")
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col=factor(classes_legend), pch=factor(classes_legend))
  }
}

##############################################################################

# helper function
# input validation
scatter_input_checks <- function(obj, x, y, kind) {
  # check object supplied
  if (class(obj) != "dbscan") {
    stop("Supplied object must be of class 'dbscan'.")
  }
  
  # check x. if a number, must be a scalar; if a character, then it must be column name in the dataset
  if (length(x) > 1 | (!is.numeric(x) & !is.character(x))) {
    stop("x must be an index referring to a column, or a column name.")
  }
  if (is.numeric(x)) {
    if (x <= 0 | x > ncol(obj$dataset)) {
      stop("If x is an index referring to a column, it must be 1 <= x <= (number of dataset columns).")
    }
  }
  else if (is.character(x)) {
    if (!(x %in% colnames(obj$dataset))) {
      stop("If x is a column name, it must exist in dataset.")
    }
  }
  
  # check y
  if (length(y) > 1 | (!is.numeric(x) & !is.character(x))) {
    stop("y must be an index referring to a column, or a column name.")
  }
  if (is.numeric(y)) {
    if (y <= 0 | y > ncol(obj$dataset)) {
      stop("If y is an index referring to a column, it must be 1 <= y <= (number of dataset columns).")
    }
  } else if (is.character(y)) {
    if (!(y %in% colnames(obj$dataset))) {
      stop("If y is a column name, it must exist in dataset.")
    }
  }
  
  # check grouping type
  if (!(kind %in% c("colour", "shape", "both"))) {
    stop("Options for 'kind' are 'colour', 'shape', and 'both'.")
  }
  # pch can only handle a maximum of 21 different symbols (26 actually, but the last 5 is just different
  # colour of the others)
  if (kind %in% c("shape", "both")) {
    if (length(unique(obj$cluster_labs)) > 21) {
      stop("Too many classes; R has a maximum of 21 different point symbols for plotting.
           Use 'kind=colour' instead.")
    }
  }
}