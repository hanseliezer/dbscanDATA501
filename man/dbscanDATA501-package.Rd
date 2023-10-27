\name{dbscanDATA501-package}
\alias{dbscanDATA501}
\docType{package}
\title{
  Density-based Spatial Clustering of Applications with Noise (DBSCAN)
}
\description{
  A basic implementation of the DBSCAN algorithm. Obtain clusters from datasets, calculate
  summary statistics, and plot dataset with the generated clusters.
  \itemize{
    \item{The main DBSCAN algorithm can be applied using the \code{dbscan()} function.}
    \item{Summary statistics on the generated clusters can be calculated and displayed through \code{summary()}.}
    \item{The clusters can be displayed on the data using a 2-D scatter plot using \code{plot()}.}
  }
}
\author{
Hans Eliezer: eliezehans@myvuw.ac.nz
}
\keyword{clustering dbscan}

\examples{
\dontrun{
blobs <- read.csv('dat/blobs.csv')
blobs_dbscan <- dbscan(blobs, eps=0.2, min_pts=5)
}
}
