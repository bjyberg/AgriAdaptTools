#' Write a Cloud-Optimized GeoTIFF (COG) from a raster file.
#'
#' This function takes a raster file, generates overviews for it, and
#' writes a Cloud-Optimized GeoTIFF (COG) to the specified output directory.
#'
#' @param x A SpatRaster or a character string specifying the path to a raster file
#' @param filename A character string specifying the output directory where the COG
#'   will be saved. If NULL (default), the COG will be saved in the same directory
#'   as the source file using the same name with a "_cog" suffix. Required if x
#'   is a SpatRaster
#'
#' @return NULL. The function generates a COG file and saves it to the specified
#'   location.
#'
#' @importFrom sf gdal_addo gdal_utils
#' @importFrom stats runif
#' @importFrom tools file_path_sans_ext
#' @importFrom terra sources
#'
#' @examples
#' # Create a COG from a source raster file with default parameters
#' \dontrun{
#' write_cog("path/to/source_raster.tif")
#' }
#' #
#' # Create a COG with a custom output directory and no flag added to the filename
#' \dontrun{
#' write_cog("path/to/source_raster.tif", out_dir = "cogs", add_flag = FALSE)
#' }
#' #
#' @export
write_cog <- function(x, filename) {
  if (inherits(x, "SpatRaster")) {
    path <- terra::sources(x)
    if (length(path) > 1 | path == "") {
      rand <- paste0(floor(stats::runif(7, min = 0, max = 10)), collapse = "")
      temp_holder <- file.path(tempdir(), paste0(rand, "gdal_terra_holder"))
      if (!dir.exists(temp_holder)) dir.create(temp_holder, recursive = T)
      temp_name <- paste0(
        floor(stats::runif(10, min = 0, max = 10)),
        collapse = "")
      terra::writeRaster(x, paste0(temp_holder, temp_name, ".tif"))
      path <- paste0(temp_holder, temp_name, ".tif")
    }
  } else if (inherits(x, "character")) {
    path <- x
  } else {
    stop("Input must be a raster or a character string file path")
  }
  if (!file.exists(path)) {
    stop("File does not exist")
  }
  if (is.null(filename) & inherits(x, "SpatRaster")) {
    stop("Filename is required when x is a SpatRaster")
  } else if (is.null(filename)) {
    out_dir <- dirname(path)
    name <- tools::file_path_sans_ext(basename(path))
    filename <- paste0(out_dir, "/", name, "_cog.tif")
    print(paste("File will be saved at:", filename))
  }
  sf::gdal_addo(
    file = path,
    overviews = c(2, 4, 8, 16),
    method = "average"
  )
  sf::gdal_utils(
    util = "translate",
    source = path,
    destination = filename,
    options = c(
      "-co", "COMPRESS=LZW",
      "-of", "COG"
    )
  )
  if (dir.exists(temp_holder)) unlink(temp_holder)
}
