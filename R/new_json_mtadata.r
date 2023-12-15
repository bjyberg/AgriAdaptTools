.files <- function(file_list, base_path) {
  rel_file <- gsub(paste0(".*", base_path), "", file_list)
  clean_rel_file <- sub("^/|^//", "", rel_file)
}

.processing_metadata <- function(process.derived_from,
    process.description,
    process.code) {
  process <- list(
    derived_from = process.derived_from,
    description = process.description,
    code = process.code
  )
  if (all(unlist(lapply(process, is.null)))) {
    process <- NULL
  }
  return(process)
}

.author <- function(
  author.name,
  author.email
) {
  if (length(author.name) != length(author.email)) {
    length(author.email) <- length(author.name)
  }
  author_df <- data.frame(
    name = author.name,
    email = author.email
  )
  return(author_df)
}

.source <- function(
  source.license,
  source.citation,
  source.url,
  source.doi
) {
  source <- list(
    licence = source.license,
    citation = source.citation,
    sourceURL = source.url,
    doi = source.doi
  )
  if (all(unlist(lapply(source, is.null)))) {
    source <- NULL
  }
  return(source)
}

.dict <- function(
  key,
  value
  ) {
  dict <- list()
  dict[[key]] <- value
  return(dict)
}

.assets <- function() { #TODO: add custom fields?
  add_asset <- menu(c("Yes", "No"),
    title = "Add an asset (i.e., metadata, technical documentation, etc.)?")
  if (add_asset == 1) {
    assets <- list()
    repeat {
      repeat {
        asset_type <- readline(prompt = "Enter asset type: ")
        asset_name <- readline(prompt = "Enter asset name: ")
        asset_path <- readline(prompt = "Enter asset path: ")
        asset_description <- readline(prompt = "Enter asset description: ")
        asset <- .dict(asset_name,
          list(description = asset_description, path = asset_path))
        print(paste("type:", asset_type, " name:", asset_name,
          " path:", asset_path, " description:", asset_description))
        correct <- menu(c("Yes", "No"),
          title = paste("Is the above correct?"))
        if (correct == 1) {
          break
        }
      }
      assets[[asset_type]] <- append(assets[[asset_type]], asset)
      # assets <- list(assets, asset)
      add_another <- menu(c("Yes", "No"), title = "Add another asset?")
      if (add_another == 2) {
        break
      }
    }
    return(assets)
  }
}

.spatial_coverage <- function(path, coverage.region) {
  if (!file.exists(path)) {
    stop(paste("Unable to find one of the file paths. Please try again."))
  }
  try(
    {
      coverage_spat <- terra::vect(path)
    },
    silent = T
  )
  try(
    {
      coverage_spat <- terra::rast(path)
    },
    silent = T
  )
  coverage <- list()
  if (!exists("coverage_spat")) {
    coverage$region <- coverage.region
    return(coverage)
  }
  coverage$region <- coverage.region
  coverage$crs$epsg <- paste0(
    terra::crs(coverage_spat, describe = T)[c("authority", "code")],
    collapse = ":")
  coverage$crs$proj <- terra::crs(coverage_spat, proj = T)
  coverage$crs$Xresolution <- terra::res(coverage_spat)[1]
  coverage$crs$Yresolution <- terra::res(coverage_spat)[2]
  coverage$crs$resolutionUnit <- "decimal-degrees"
  coverage$extent$xmin <- terra::ext(coverage_spat)$xmin
  coverage$extent$xmax <- terra::ext(coverage_spat)$xmax
  coverage$extent$ymin <- terra::ext(coverage_spat)$ymin
  coverage$extent$ymax <- terra::ext(coverage_spat)$ymax
  return(coverage)
}

.temporal_coverage <- function(temporal.resolution, temporal.start_date, temporal.end_date) {
  coverage <- list()
  coverage$temporal$resolution <- ""
  coverage$temporal$start_date <- ""
  coverage$temporal$end_date <- ""
  return(coverage)
}

.layers <- function(path, stats = T, hist = T) {
  type <- tools::file_ext(path)
  type <- tolower(type)
  if (type %in% c("tif", "tiff")) { # TODO: add and check ncdf
    x <- terra::rast(path)
  } else if (type %in% c("shp", "gpkg", "geojson")) {
    x <- as.data.frame(terra::vect(path))
  } else if (type == "csv") {
    x <- read.csv(path)
  } else if (type == "tsv") {
    x <- read.delim(path)
  } else if (type == "rds") {
    x <- readRDS(path) # CHECK: do we want to keep if no way of knowing what it will open?
  } else if (type == "xls" || type == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) { # TODO: add optional dependency
      stop('install "readxl" to get column attributes from Excel files')
    }
    x <- as.data.frame(readxl::read_excel(path))
  } else if (type == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) { # TODO: add optional dependency
      stop('install "arrow" to get column attributes from parquet files')
    }
    # add parquet function and check it is same as names()/hist() to DF
  } else if (type == "parquet") {
    if (!requireNamespace("geoarrow", quietly = TRUE)) { # CHECK: geoarrow or arrow SF? has terra/gdal implemented it? same file name as parquet. what happens if parquet is opened in geoarrow?
      stop('install "geoarrow" to get column attributes from geoparquet files')
    }
    # add geoparquet function and check it is same as names()/hist() to DF
  } else {
    stop(paste(type, "is currently unsupported. No data will be added.")) # TODO: come up with a better warning/way of calling them attributs/layers
  }
  # TODO: need to make the stops only stop this function, not the full process. probably use tryCatch
  x_names <- names(x) # TODO: check if works for all data types (spatRaster??)

  if (stats) {
    if (inherits(x, "spatRaster")) {
      stats_df <- global(x, c("mean", "sd", "min", "max"), na.rm = T)
      stats_df$name <- row.names(stats_df)
      stats_df$dtype <- datatype(x)
    } else if (inherits(x, "data.frame")) {
      df_list <- list()
      for (i in seq_along(x)) {
        cName <- x_names[i]
        if (is.numeric(x[[i]])) {
          cMin <- min(x[[i]], na.rm = T)
          cMax <- max(x[[i]], na.rm = T)
          cMean <- mean(x[[i]], na.rm = T)
          cSD <- sd(x[[i]], na.rm = T)
          dtype <- class(x[[i]])
          cAttrs <- c(cMean, cSD, cMin, cMax, cName, dtype)
        } else {
          dtype <- class(x[[i]])
          cAttrs <- list(NA, NA, NA, NA, cName, dtype) # TODO: NA or Null?
        }
        df_list[[i]] <- cAttrs
      }
      as.data.frame(do.call(rbind, df_list))
      names(df_list) <- c("mean", "sd", "min", "max", "name", "dtype")
    }
  }
  if (hist) {
    histos <- list()
    if (inherits(x, "data.frame")) {
      hist_cols <- names(x)[sapply(x, is.numeric)]
      for (i in hist_cols) {
        histos[[i]] <- hist(x[[i]])
      }
    } else if (inherits(x, "spatRaster")) {
        histos <- hist(x)
    }
  }
}