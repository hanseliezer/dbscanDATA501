# fully possible that the clustering results lead to errors returned by these validation metric
# functions, so need to wrap them in a tryCatch to prevent summary() from stopping altogether.
# in case of warnings/errors, just return NaN
connectivity_wrapper <- function(data, labs) {
  score <- tryCatch(clValid::connectivity(Data=data, clusters=labs),
                    error=function(e) { return(NaN) },
                    warning=function(w) { return(NaN) })
  
  return(score)
}

silhouette_wrapper <- function(data, labs) {
  dist_mat <- dist(data)
  silh <- tryCatch(cluster::silhouette(x=labs, dist=dist_mat),
                   error=function(e) { return(NaN) },
                   warning=function(w) { return(NaN) })
  
  # silhouette returns NA for 'trivial clustering' (either only 1 or all N clusters)
  score <- if (length(silh) > 1) mean(silh[, 'sil_width']) else NaN
  return(score)
}

dunn_wrapper <- function(data, labs) {
  score <- tryCatch(clValid::dunn(Data=data, clusters=labs),
                    error=function(e) { return(NaN) },
                    warning=function(w) { return(NaN) })
  
  return(score)
}

cdbw_wrapper <- function(data, labs) {
  score <- tryCatch(fpc::cdbw(x=data, clustering=labs)$cdbw,
                    error=function(e) { return(NaN) },
                    warning=function(w) { return(NaN) })
  
  return(score)
}

