#' Demonstration of DBSCAN on toy datasets
#' 
#' @description
#' Applies DBSCAN to 2-D toy datasets and displays the results. `blobs` represents points collected into
#' two clearly-separated approximately-globular clusters, and `circles` represents points collected into
#' two hollow  circles inside each other. Three different algorithms will be fitted into the dataset,
#' each with different `min_pts` and `eps` parameters, clustering summaries printed, and scatter plots
#' drawn to display the resulting clusters to demonstrate the impact of the different parameters.
#' 
#' @returns There are two parts:
#' \itemize{
#'  \item Printed summary table of clustering results: number of clusters generated (excluding noise),
#'  and the four validation metrics used in `summary()`: connectivity, mean silhouette width, Dunn index,
#'  and CDbw.
#'  \item Scatter plots of the data, with generated clusters displayed as different colours.
#' }
#' 
#' @keywords clustering
#' @examples
#' \dontrun{
#' demo_blobs()
#' demo_circles()
#' }
#' @rdname demo
#' @export
demo_blobs <- function() {
  blobs_eps <- c(0.01, 0.25, 1)
  blobs_min_pts <- c(2, 5, 10)
  demo(dbscanDATA501::blobs, blobs_eps, blobs_min_pts)
}

#' @rdname demo
#' @export
demo_circles <- function() {
  circles_eps <- c(0.075, 0.1, 0.25)
  circles_min_pts <- c(3, 5, 10)
  demo(dbscanDATA501::circles, circles_eps, circles_min_pts)
}

##############################################################################

# helper functions
# collate everything into one list
demo_summary <- function(data, eps, min_pts) {
  # get algorithm results
  obj <- dbscanDATA501::dbscan(data, eps, min_pts)
  # calculate number of clusters, excluding noise
  clust_nums <- unique(obj$cluster_labs[obj$cluster_labs != 0])
  n_clusters <- if (length(clust_nums) > 0) length(clust_nums) else 0
  
  # calculate validation metrics
  score_conn <- connectivity_wrapper(obj$dataset, obj$cluster_labs)
  score_silh <- silhouette_wrapper(obj$dataset, obj$cluster_labs)
  score_dunn <- dunn_wrapper(obj$dataset, obj$cluster_labs)
  score_cdbw <- cdbw_wrapper(obj$dataset, obj$cluster_labs)
  
  # create classes for plot and it's legend
  classes <- replace(obj$cluster_labs, obj$cluster_labs == 0, "Noise")
  classes <- factor(classes)
  classes_legend <- levels(classes)
  
  return_list <- list('n_clusters'=n_clusters,
                      'score_conn'=round(score_conn, 3),
                      'score_silh'=round(score_silh, 3),
                      'score_dunn'=round(score_dunn, 3),
                      'score_cdbw'=round(score_cdbw, 3),
                      'plot_classes'=classes,
                      'plot_classes_legend'=classes_legend)
  
  return(return_list)
}

# print clustering summary and create plots
demo <- function(data, eps_list, min_pts_list) {
  # run algorithm on data with different pairs of parameters
  cluster_1 <- demo_summary(data, eps_list[1], min_pts_list[1])
  cluster_2 <- demo_summary(data, eps_list[2], min_pts_list[2])
  cluster_3 <- demo_summary(data, eps_list[3], min_pts_list[3])
  
  # table of summary values to be printed
  cluster_summary <- data.frame(n_cluster=c(cluster_1$n_clusters,
                                            cluster_2$n_clusters,
                                            cluster_3$n_clusters),
                                conn=c(cluster_1$score_conn,
                                       cluster_2$score_conn,
                                       cluster_3$score_conn),
                                silh=c(cluster_1$score_silh,
                                       cluster_2$score_silh,
                                       cluster_3$score_silh),
                                dunn=c(cluster_1$score_dunn,
                                       cluster_2$score_dunn,
                                       cluster_3$score_dunn),
                                cdbw=c(cluster_1$score_cdbw,
                                       cluster_2$score_cdbw,
                                       cluster_3$score_cdbw))
  # give proper column names
  names(cluster_summary) <- c("No. clusters (excl. noise)", "Connectivity", "Mean silhouette width",
                              "Dunn index", "CDbw")
  # parameter values as row names
  param_names <- c(paste0("eps=", eps_list[1], ", min_pts=", min_pts_list[1]),
                   paste0("eps=", eps_list[2], ", min_pts=", min_pts_list[2]),
                   paste0("eps=", eps_list[3], ", min_pts=", min_pts_list[3]))
  rownames(cluster_summary) <- param_names
  
  # print a clean table
  cat("Clustering summary:\n")
  print(cluster_summary)
  
  par(mar=c(5.1, 4, 4, 5.25), xpd=TRUE)
  # use plot.default rather than plot.dbscanDATA501 as to give it a different axes labels
  # and title
  plot.default(data$X, data$Y, col=cluster_1$plot_classes, pch=20,
               xlab="X", ylab="Y", main=param_names[1])
  legend("topleft", inset=c(1, 0), legend=cluster_1$plot_classes_legend,
         col=factor(cluster_1$plot_classes_legend), pch=20)
  
  plot.default(data$X, data$Y, col=cluster_2$plot_classes, pch=20,
               xlab="X", ylab="Y", main=param_names[2])
  legend("topleft", inset=c(1, 0), legend=cluster_2$plot_classes_legend,
         col=factor(cluster_2$plot_classes_legend), pch=20)
  
  plot.default(data$X, data$Y, col=cluster_3$plot_classes, pch=20,
               xlab="X", ylab="Y", main=param_names[3])
  legend("topleft", inset=c(1, 0), legend=cluster_3$plot_classes_legend,
         col=factor(cluster_3$plot_classes_legend), pch=20)
}