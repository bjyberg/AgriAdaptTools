#' Upload a file to an S3 bucket
#'
#' This function uploads a file to the specified S3 bucket. If the file extension is "tif" and the `cog` parameter is set to TRUE, the function converts the file to a COG (Cloud-Optimized GeoTIFF) before uploading.
#'
#' @param localpath A character string specifying the path to the local file
#' @param bucketpath A character string specifying the path to the destination bucket in S3
#' @param cog A logical value indicating whether to convert a tif to a COG before uploading (default is TRUE). 
#' @param ... Additional arguments passed to the `s3fs::s3filesystem_class file_upload` function
#'
#' @return An object representing the uploaded file in S3
#' 
#' @examples
#' \dontrun{
#' # setting credentials is only required if there is no config file saved to "~/.aws/config"
#'  Sys.setenv(
#'     AWS_ACCESS_KEY_ID = "your AWS access key",
#'     AWS_SECRET_ACCESS_KEY = "your AWS secret key",
#'     AWS_DEFAULT_REGION = "your AWS region" (optional),
#'     OR
#'     AWS_CONFIG_FILE = "~/.aws/config" (or ~/path/to/aws/config/file),
#'  )
#' file_to_s3(localpath = "path/to/local/file.tif", bucketpath = "path/to/bucket/file.tif", cog = TRUE, overwrite = FALSE)
#' 
#' # or for a full folder upload
#' folder_list <- list.files("path/to/folder", recursive = TRUE, pattern = "\\.tif|\\.gpkg")
#' for (i in seq_along(folder_list)) {
#'   file_to_s3(localpath = folder_list[i],
#'   bucketpath = paste0("path/to/bucket/", folder_list[i]),
#'   cog = TRUE, overwrite = FALSE)
#' }
#'}
#' @importFrom tools file_ext
#' @importFrom s3fs S3FileSystem
#' @export
file_to_s3 <- function(localpath, bucketpath, cog = TRUE, ...) {
  if (tools::file_ext(localpath) == "tif" & cog) {
      tmp.dir <- file.path(tempdir(), "tempcog")
      if (!dir.exists(tmp.dir)) dir.create(tmp.dir)
      write_cog(localpath, file.path(tmp.dir, "tempcog.tif"))
      localpath <- file.path(tmp.dir, "tempcog.tif")
  }
  s3 <- s3fs::S3FileSystem$new()
  s3_file <- s3$file_upload(localpath, bucketpath, ...)
  if (exists("tmp.dir")) {
    if (dir.exists(tmp.dir)) unlink(tmp.dir, recursive = TRUE)
  }
  return(s3_file)
}

# to do:
# - keep folder structure/make new folders if needed to keep dir structure
# - full folder adn subfolder upload that allows cog converstion (or add example)