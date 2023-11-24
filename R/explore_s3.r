library(aws.s3)
library(jsonlite)
library(data.tree)
library(s3fs)

explore_s3 <- function(
    bucket.path = NULL,
    public = FALSE,
    region = "us-east-1",
    recursive = TRUE,
    return_type = c("list", "json", "data.frame", "tree")) {
  return_type <- match.arg(return_type)
  print(return_type)
  if (public) {
    s3 <- S3FileSystem$new(region_name = region, anonymous = T)
  } else {
    tryCatch(
      {
        s3 <- S3FileSystem$new(region_name = region)
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
  tree <- as.Node(tree_df)
  tree_list <- as.list(tree)
  View(tree_list)
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

explore_s3()
explore_s3("herbariumnsw-pds", public = TRUE, region = "ap-southeast-2", recursive = FALSE)





#   file_df <- aws.s3::get_bucket_df(s3.bucket, max = Inf,
#     prefix = root, region = region)
#    file_df <- aws.s3::get_bucket_df("digital-atlas", max = Inf,
#      prefix = "atlas-data")
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

# library(paws)
# paws::
# s3 <- paws::s3(list(region = "us-east-1"))
# s3_data <- s3$list_objects_v2(Bucket = 'digital-atlas', Prefix = 'atlas-data')
# s3_data <- s3_data[["Contents"]]
# s3_data
# explore_s3(s3.bucket = "s3://herbariumnsw-pds/", region = "ap-southeast-2")
# herbarium_files <- get_bucket_df(
#   bucket = "herbariumnsw-pds",
#   region = "ap-southeast-2",
#   max = 20000
# )

# library(s3fs)

# herb <- S3FileSystem$new(region_name = "ap-southeast-2", anonymous = T)
# s3_dir_info("digital-atlas/atlas-data")

# herb$dir_info("herbariumnsw-pds") 
# file_df <- as.data.frame(herb$dir_info("herbariumnsw-pds"))
# file_df$pathString <- paste0("herbariumnsw-pds/", file_df$key)
# data.tree::as.Node(file_df)

# file_df$size_mb <- file_df$size / 1000000
# tree_df <- file_df[c("pathString", "size_mb", "last_modified")]
# tree <- as.Node(tree_df)
# tree_list <- as.list(tree)
# View(tree_list)
