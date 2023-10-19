#' Toy datasets
#'
#' @description
#' There are two toy datasets provided in this package: `blobs` and `circles`. `blobs` have points
#' grouped into two different clusters with approximately globular shape, while `circles` are grouped
#' into two clusters shaped like a hollow circle with one being inside the other. They are used by the
#' demonstration functions `demo_blobs()` and `demo_circles()` respectively, as well as being accessible
#' in general through `data(blobs)` and `data(circles)`.
#' 
#' Both datasets are generated in Python using `scikit-learn`'s `make_blobs()` ("make_blobs" 2023) and
#' `make_circles()` ("make_circles" 2023).
#'
#' @format ## `blobs`
#' A data frame with 500 rows and 2 columns:
#' \describe{
#'   \item{X}{X coordinates}
#'   \item{Y}{Y coordinates}
#' }
#' 
#' @references 
#' “Make_blobs.” 2023. scikit-learn.
#' https://scikit-learn.org/stable/modules/generated/sklearn.datasets.make_blobs.html.
#' 
#' “Make_circles.” 2023. scikit-learn.
#' https://scikit-learn.org/stable/modules/generated/sklearn.datasets.make_circles.html.
#' 
#' @rdname data
"blobs"

#' @format ## `circles`
#' A data frame with 1,000 rows and 2 columns:
#' \describe{
#'   \item{X}{X coordinates}
#'   \item{Y}{Y coordinates}
#' }
#' @rdname data
"circles"