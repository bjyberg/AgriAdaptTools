#' MinMax_norm
#'
#' This function normalizes the input vector or SpatRaster object using the min-max normalization technique.
#' If the input is a SpatRaster, the function calculates the minimum and maximum values and performs normalization.
#' If the input is a numeric vector, the function calculates the minimum and maximum values and performs normalization.
#'
#' @param x A numeric vector or SpatRaster to be normalized.
#' @param inverse Logical value indicating whether to apply inverse the values
#' @return A normalized numeric vector or SpatRaster object.
#' @importFrom terra minmax
#' @export

MinMax_norm <- function(x, inverse = FALSE) {
  if (inherits(x, "SpatRaster")) {
    mnmx <- terra::minmax(x, compute = TRUE)
    if (inverse) {
      x <- mnmx[1, ] - x + mnmx[2, ]
    }
    norm <- (x - mnmx[1, ]) / (mnmx[2, ] - mnmx[1, ])
    return(norm)
  } else if (inherits(x, "numeric")) {
  } else {
    stop("X must be a SpatRaster or a numeric vector")
  }
}
