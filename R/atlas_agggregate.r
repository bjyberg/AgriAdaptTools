#' atlas_aggregate
#'
#' This function normalizes the input vector or SpatRaster object using the min-max normalization technique.
#' If the input is a SpatRaster, the function calculates the minimum and maximum values and performs normalization.
#' If the input is a numeric vector, the function calculates the minimum and maximum values and performs normalization.
#'
#' @param x A SpatRaster or path to raster layer
#' @param admin_level A character string indicating the administrative level to aggregate to
#' @param fun The aggregation function used by exact_extract
#' @param colname The column name in the output dataframe. If missing, it is inherited from the raster
#' @param ... Additional arguments passed to exact_extract
#' @return An sf object containing the aggregated data
#' @importFrom terra rast nlyr
#' @importFrom exactextractr exact_extract
#' @importFrom sf read_sf
#' @seealso \code{\link[exactextractr]{exact_extract}}
#' @export

atlas_aggregate <- function(x, admin_level = c('admin0', 'admin1', 'admin2'),
                            fun, colname = NULL, ...) {
  admin_level <- match.arg(admin_level)
  # admin <- sfarrow::st_read_parquet(
  #   paste0(
  #     "s3://digital-atlas/boundaries/atlas-region_",
  #     admin_level,
  #     ".parquet"
  #   )
  # )
  admin <- sf::read_sf(
    paste0(
      "/vsis3/digital-atlas/boundaries/atlas-region_", admin_level, "_harmonized.gpkg"
    )
  )
  print(paste('Extracting data for admin', admin_level, 'using', fun))
  if (inherits(x, "character")) {
    x <- terra::rast(x)
  }
  if (is.null(colname)) {
  colname <- names(x)[1]
  print(paste('Assuming column name from x. Using', colname, 'as column name'))
  }
  if (inherits(x, "SpatRaster")) {
    if (terra::nlyr(x) == 1) {
      admin[colname] <- exactextractr::exact_extract(x, admin, fun = fun, ...)
    } else if (terra::nlyr(x) > 1) {
      xtract_df <- exactextractr::exact_extract(x, admin, fun = fun, ...)
      admin <- cbind(admin, xtract_df)
    }
  } else {
    stop("x must be a SpatRaster or a filepath to a raster (local or cloud)")
  }
  return(admin)
}