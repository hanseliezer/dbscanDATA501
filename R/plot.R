#' Scatter plot of DBSCAN clusters
#' 
#' @description
#' Displays dataset on a 2-D scatter plot with the DBSCAN-generated clusters as the grouping variable.
#' 
#' @param x Object of class `dbscanDATA501`; will contain the dataset and cluster labels.
#' @param ax1 Feature to be displayed on x-axis. Either column index or name. Defaults to first column.
#' @param ax2 Feature to be displayed on y-axis. Either column index or name. Defaults to second column.
#' @param kind How clusters are displayed as the grouping of data points. Default is `colour` (i.e.
#' displays the clusters with different colours); other options are `shape` and `both` (colour *and*
#' shape).
#' @param ... Graphical parameters to be passed to `plot.default()`, or arguments for other methods.
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' data(blobs)
#' dbs <- dbscan(blobs, 5, 0.2)
#' plot(dbs)
#' }
#' @rdname plot
#' @export
plot.dbscanDATA501 <- function(x, ax1=1, ax2=2, kind='colour', ...) {
  # input validation
  scatter_input_checks(x, ax1, ax2, kind, ...)
  
  # replace 0 labels with 'Noise'
  classes <- replace(x$cluster_labs, x$cluster_labs == 0, "Noise")
  classes <- factor(classes)
  classes_legend <- levels(classes)
  
  # xpd allows stuff to be displayed outside the initial plot
  par(mar=c(5.1, 4, 4, 5.25), xpd=TRUE)
  
  if (kind == 'colour') {
    plot.default(x=x$dataset[, ax1], y=x$dataset[, ax2], type="p",
                 col=classes, pch=1, ...)
    # legend location is top left, but inset shifts it 101% to the right so now it's
    # placed top right just outside the plot. this way we don't have to bother setting
    # exact coordinates which can be iffy when given data of different scales
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col=factor(classes_legend), pch=1)
  }
  else if (kind == 'shape') {
    plot.default(x=x$dataset[, ax1], y=x$dataset[, ax2], type="p",
                 col="red", pch=as.numeric(classes), ...)
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col="red", pch=factor(classes_legend))
  }
  else if (kind == 'both') {
    plot.default(x=x$dataset[, ax1], y=x$dataset[, ax2], type="p",
                 col=classes, pch=as.numeric(classes), ...)
    legend("topleft", inset=c(1, 0), legend=classes_legend,
           col=factor(classes_legend), pch=factor(classes_legend))
  }
}

##############################################################################

# helper function
# input validation
scatter_input_checks <- function(x, ax1, ax2, kind, ...) {
  
  # check ax1. if a number, must be a scalar; if a character, then it must be column name in the dataset
  if (length(ax1) > 1 | (!is.numeric(ax1) & !is.character(ax1))) {
    stop("ax1 must be an index referring to a column, or a column name.")
  }
  if (is.numeric(ax1)) {
    if (ax1 <= 0 | ax1 > ncol(x$dataset)) {
      stop("If ax1 is an index referring to a column, it must be 1 <= ax1 <= (number of dataset columns).")
    }
  }
  else if (is.character(ax1)) {
    if (!(ax1 %in% colnames(x$dataset))) {
      stop("If ax1 is a column name, it must exist in dataset.")
    }
  }
  
  # check ax2
  if (length(ax2) > 1 | (!is.numeric(ax2) & !is.character(ax2))) {
    stop("ax2 must be an index referring to a column, or a column name.")
  }
  if (is.numeric(ax2)) {
    if (ax2 <= 0 | ax2 > ncol(x$dataset)) {
      stop("If ax2 is an index referring to a column, it must be 1 <= ax2 <= (number of dataset columns).")
    }
  } else if (is.character(ax2)) {
    if (!(ax2 %in% colnames(x$dataset))) {
      stop("If ax2 is a column name, it must exist in dataset.")
    }
  }
  
  # check grouping type
  if (length(kind) > 1) {
    stop("Please select one of 'colour', 'shape', and 'both'.")
  }
  else if (!(kind %in% c("colour", "shape", "both"))) {
    stop("Options for 'kind' are one of 'colour', 'shape', and 'both'.")
  }
  
  # pch can only handle a maximum of 21 different symbols (26 actually, but the last 5 is just different
  # colour of the others)
  if (kind %in% c("shape", "both")) {
    if (length(unique(x$cluster_labs)) > 21) {
      stop("Too many classes; R has a maximum of 21 different point symbols for plotting.
           Use 'kind=colour' instead.")
    }
  }
}
