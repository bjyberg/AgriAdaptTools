#' Write a Cloud-Optimized GeoTIFF (COG) from a source raster file.
#'
#' This function takes a source raster file, generates overviews for it, and
#' writes a Cloud-Optimized GeoTIFF (COG) to the specified output directory.
#'
#' @param path A character string specifying the path to the source raster file.
#' @param out_dir A character string specifying the output directory where the COG
#'   will be saved. If NULL (default), the COG will be saved in the same directory
#'   as the source file.
#' @param add_flag A logical value indicating whether to add "_cog" to file's
#'   name. Default is TRUE.
#'
#' @return NULL. The function generates a COG file and saves it to the specified
#'   location.
#'
#' @importFrom sf gdal_addo gdal_utils
#' @import stars
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' # Create a COG from a source raster file with default parameters
#' \dontrun{write_cog("path/to/source_raster.tif")}
#' #
#' # Create a COG with a custom output directory and no flag added to the filename
#' \dontrun{write_cog("path/to/source_raster.tif", out_dir = "cogs", add_flag = FALSE)}
#' #
#' @export
write_cog <- function(path, out_dir = NULL, add_flag = TRUE) {
  if (!file.exists(path)) {
    stop("File does not exist")
  }
  if (is.null(out_dir)) {
    out_dir <- dirname(path)
  }
  if (add_flag) {
    flag <- "_cog"
  } else {
    flag <- ""
  }
  file_name <- tools::file_path_sans_ext(basename(path))
  sf::gdal_addo(
    file = path,
    overviews = c(2, 4, 8, 16),
    method = "average"
  )
  sf::gdal_utils(
    util = "translate",
    source = path,
    destination = file.path(out_dir, paste0(file_name, flag, ".tif")),
    options = c(
      "-co", "COMPRESS=LZW",
      "-of", "COG"
    )
  )
}