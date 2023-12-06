#' Resample Raster Data to Adaptation Atlas Grid (5 arc min)
#'
#' This function resamples a given raster object to a standard atlas grid with a specified resampling method.
#'
#' @param x An input raster object or a data structure that can be
#'   converted to a spatial raster (e.g., a matrix or data frame).
#' @param fun The resampling method to use. Default is "bilinear," but other methods
#'   supported by the 'terra' package (e.g., "sum", "bilinear" "cubic", "mean") can be specified.
#' @param ... Additional arguments to be passed to the terra::resample function.
#'
#' @return A resampled raster object on the standard atlas grid.
#' 
#' @examples
#' # Example 1: Resample a raster using the default method
#'  \dontrun{result <- atlas_resample(input_raster)}
#' #
#' # Example 2: Resample a raster using a specified resampling method
#'  \dontrun{result <- atlas_resample(input_raster, fun = "sum")}
#' 
#' @seealso
#' [terra::resample()] for detailed information on the resampling process
#' 
#' @importFrom terra rast resample
#'
#' @export

atlas_resample <- function(x, fun = "bilinear", ...) {
  if (!inherits(x, "SpatRaster")) {
    simpleError("Input raster must be of class 'SpatRaster'.")
  }
  base <- rast(resolution = (5 / 60), crs = "epsg:4326",
    xmin = -26, xmax = 58, ymin = -47, ymax = 41,
  )
  resampled <- resample(x, base, method = fun, ...)
  print(resampled)
  return(resampled)
}