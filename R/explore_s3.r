#' Explore S3 Bucket
#'
#' This function is used to explore the contents of a S3 bucket. It is designed to return an object or viewer that
#' is as clos to a normal directory tree as possible. It can also be used to return a dataframe that contiains the 
#' data and structure of the bucket. 
#' 
#' opens a list of AWS S3 objects in a new window that is in the format of a collapsible directory tree shape
#'
#' @param bucket.path The path of the S3 bucket to explore. If not provided, a default path to the "Adaptation Atlas" data will be used
#' @param public Logical value indicating whether the bucket is public or not. If the bucket it public, it prevents the use of AWS credentials avoiding an error.
#' @param region The AWS region where the bucket is located. Default is "us-east-1".
#' @param recursive Logical value indicating whether to explore the bucket recursively or not. Default is TRUE.
#' @param return_type The format in which the information should be returned. Possible values are "list", "json", "data.frame", "tree". Default is "list".
#' @param open.view Logical value indicating whether to open the list in a new window or not. Default is TRUE.
#' 
#' @return Depending on the specified return type, the function returns a list, hierarchical JSON, data frame, or tree object.
#'
#' @examples
#' explore_s3(bucket.path = "noaa-gsod-pds", public = TRUE, recursive = FALSE, open.view = FALSE)
#' 
#' \dontrun{
#'  Sys.setenv(
#'     AWS_ACCESS_KEY_ID = "your AWS access key",
#'     AWS_SECRET_ACCESS_KEY = "your AWS secret key",
#'     AWS_DEFAULT_REGION = "your AWS region" (optional),
#'     OR
#'     AWS_CONFIG_FILE = "~/.aws/config" (or ~/path/to/aws/config/file),
#'  )
#' explore_s3() # this will default to the Adaptation Atlas Data
#' }
#' @importFrom s3fs S3FileSystem
#' @importFrom data.tree as.Node
#' @importFrom jsonlite toJSON
#' @importFrom utils View
#'
#' @export
explore_s3 <- function(
    bucket.path = NULL,
    public = FALSE,
    region = "us-east-1",
    recursive = TRUE,
    open.view = TRUE,
    return_type = c("list", "json", "data.frame", "tree")) {
  return_type <- match.arg(return_type)
  if (public) {
    s3 <- s3fs::S3FileSystem$new(region_name = region, anonymous = T)
  } else {
    tryCatch(
      {
        s3 <- s3fs::S3FileSystem$new(region_name = region)
      },
      error = function(e) {
        stop("Could not connect to S3. Check your AWS credentials and region")
      })
  }
  if (is.null(bucket.path)) {
    bucket.path = "digital-atlas/atlas-data"
  }
  file_df <- as.data.frame(s3$dir_info(bucket.path, recurse = recursive))
  file_df$pathString <- paste0(file_df$bucket, "/", file_df$key)
  file_df$size_mb <- file_df$size / 1000000
  tree_df <- file_df[c("pathString", "size_mb", "last_modified", "uri")]
  tree <- data.tree::as.Node(tree_df)
  tree_list <- as.list(tree)
  if (open.view) {
    utils::View(tree_list)
  }
  if (return_type == "json") {
    tree_json <- jsonlite::toJSON(tree_list, pretty = TRUE, na = "null")
    return(tree_json)
  }
  if (return_type == "data.frame") {
    return(tree_df)
  }
  if (return_type == "tree") {
    return(tree)
  }
  if (return_type == "list") {
    return(tree_list)
  }
}

# old function based oon the now abandoned aws.s3 library
# explore_s3 <- function(s3.bucket, root, region = "us-east-1") {
#   file_df <- aws.s3::get_bucket_df(s3.bucket, max = Inf,
#     prefix = root, region = region)
#   file_df$pathString <- file_df$Key
#   file_df$size_mb <- as.numeric(file_df$Size) / 1000000
#   tree_df <- file_df[c("pathString", "size_mb", "LastModified")]
#   full_path <- paste(file_df$Bucket, root, file_df$Key, sep = "/")
#   tree_df$s3_path <- paste0("s3://", full_path)
#   tree2 <- as.Node(tree_df)
#   tree_list <- as.list(tree)
#   # tree_json <- jsonlite::toJSON(tree_list, pretty = TRUE, na = "null")
#   View(tree_list)
#   return(tree_list)
# }
