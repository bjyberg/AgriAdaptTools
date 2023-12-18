collection_metadata <- function( #add an ability to update the metadata
  collection.ID,
  collection.path,
  collection.description = NULL,
  metadata.author = NULL,
  metadata.author.email = NULL,
  keywords = NULL,
  # These are shared between folder and item groups folder takes priority, so if they are assigned here, they can't be assigned in group
  source.author = NULL,
  source.author.email = NULL,
  source.license = NULL,
  source.citation = NULL,
  source.url = NULL,
  source.doi = NULL,
  process.description = NULL,
  process.derived_from = NULL,
  process.code = NULL,
  custom_fields = NULL, # needs to be type list
  assets = list() # asset format function and dictionary function
  ) {
  out_nam <- paste0(collection.path, "/", collection.ID,
    "_digiAtlas-metadata.json") # THIS PART OF NAME HARDCODED TO FIND LATER
  
  if (file.exists(out_nam)) {
    collection.metadata <- jsonlite::read_json(out_nam, simplifyVector = T)
    collection.metadata$metadata$dateModified <- format(Sys.time(), "%Y-%m-%d")
    if (!is.null(collection.description)) {
      collection.metadata$description <- collection.description
    }
  } else {
    collection.metadata <- list()
    collection.metadata$ID <- collection.ID
    collection.metadata$metadata$dateCreated <- format(Sys.time(), "%Y-%m-%d")
    collection.metadata$metadata$dateModified <- format(Sys.time(), "%Y-%m-%d")
    collection.metadata$description <- collection.description
  }
  if (!is.null(collection.metadata$metadata$authors)) {
    new_author <- .author(metadata.author,
      metadata.author.email)
    og_author <- as.data.frame(collection.metadata$metadata$authors)
    collection.metadata$metadata$authors <- unique(rbind(og_author, new_author))
  } else {
    collection.metadata$metadata$authors <- .author(metadata.author,
      metadata.author.email)
  }
  if (!is.null(collection.metadata$keywords)) {
    old <- collection.metadata$keywords
    new <- keywords
    collection.metadata$keywords <- unique(c(old, new))
  } else {
    collection.metadata$keywords <- keywords
  }
  if (!is.null(collection.metadata$source)) {
    if (!is.null(source.license)) {
      collection.metadata$source$license <- source.license
    }
    if (!is.null(source.author)) {
      new_src_author <- .author(source.author, source.author.email)
      og_src_author <- as.data.frame(collection.metadata$source$authors)
      collection.metadata$source$authors <- unique(
        rbind(og_src_author, new_src_author)
      )
    }
    if (!is.null(source.citation)) {
      og_citaion <- collection.metadata$source$citation
      new_citaion <- unique(c(og_citaion, source.citation))
      collection.metadata$source$citation <- new_citaion
    }
    if (!is.null(source.url)) {
      og_url <- collection.metadata$source$url
      new_url <- unique(c(og_url, source.url))
      collection.metadata$source$url <- new_url
    }
    if (!is.null(source.doi)) {
      og_doi <- collection.metadata$source$doi
      new_doi <- unique(c(og_doi, source.doi))
      collection.metadata$source$doi <- new_doi
    }
  } else {
    collection.source <- .source(source.license, source.citation,
      source.url, source.doi)
    collection.metadata$source$authors <- .author(source.author,
      source.author.email)
    collection.metadata$source <- append(collection.metadata$source,
      collection.source)
  }
  if (!is.null(collection.metadata$processing)) {
    if (!is.null(process.derived_from)) {
      og_derived <- process.derived_from
      collection.metadata$processing$derived_from <- unique(c(og_derived,
        process.derived_from))
    }
    if (!is.null(process.description)) {
      collection.metadata$processing$description <- process.description
    }
    if (!is.null(process.code)) {
      og_code <- process.code
      collection.metadata$processing$code <- unique(c(og_code, process.code))
    }
  } else {
    collection.metadata$processing <- .processing_metadata(
      process.derived_from,
      process.description,
      process.code)
  }
  if (!is.null(collection.metadata$other_metadata)) {
    og_md <- collection.metadata$other_metadata
    merged_customMD <- with(
      unique(stack(c(og_md, custom_fields))),
      split(values, ind)
    )
    collection.metadata$other_metadata <-merged_customMD
  } else {
    collection.metadata$other_metadata <- custom_fields
  }
  if (!is.null(collection.metadata$assets)) {
    og_assets <- collection.metadata$assets
    new_assets <- list(og_assets, assets)
    collection.metadata$assets <- unique(new_assets)
  } else {
    collection.metadata$assets <- assets
  }
  jsonlite::toJSON(collection.metadata, pretty = T, auto_unbox = T) |>
    cat(file = out_nam)

  return(collection.metadata)
}

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
    license = source.license,
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
  coverage$temporal$resolution <- temporal.resolution
  coverage$temporal$start_date <- temporal.start_date
  coverage$temporal$end_date <- temporal.end_date
  return(coverage)
}

.layers <- function(path, stats = T, hist = T) { # TODO: add same names feature to only get names from one of a list/ group level vs file level
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
  attr_ls <- list()
  for (i in names(x)) {
    attr_ls[[i]]$name <- i
  }
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
    for (i in seq_along(hist)) {
      name <- histo[[i]]$xname
      attr_ls[[name]]$hist$num_bucket <- length(histos[[i]]$counts) # num buckets
      attr_ls[[name]]$hist$min <- min(histos[[i]]$breaks)
      attr_ls[[name]]$hist$max <- max(histos[[i]]$breaks)
      attr_ls[[name]]$hist$count <- histos[[i]]$counts
    }
  }
}



t1 <- collection_metadata(
  collection.ID = "testingSHIT70",
  collection.path = "/home/bjyberg/",
  collection.description = "CMIP6 Year Hazard Indices",
  metadata.author = "Brayden Youngberg",
  metadata.author.email = "bjyberg1@gmail.com",
  keywords = c("hazards", "cmip6"),
  source.author = c("Ramirez-Villegas, J.", "Stewart, P."),
  source.author.email = c("j.r.villegas@cgiar.org", "p.stewart@cgiar.org"),
  source.license = "CC BY 4.0",
  source.citation = "Ramirez-Villegas, J., Achicanoy, H., Thornton, P.K. 2023. CMIP6 climate hazards: human heat stress index. CGIAR. Dataset.",
  source.url = NULL,
  source.doi = NULL,
  process.description = NULL,
  process.derived_from = "/home/jovyan/common_data/atlas_hazards/cmip6/indices",
  process.code = "https://github.com/AdaptationAtlas/hazards_prototype/blob/main/R/0_make_timeseries.R")

View(t1)


t2 <- collection_metadata(
  collection.ID = "testingSHIT70",
  collection.path = "/home/bjyberg/",
  collection.description = "CMIP6 Year Hazard Indices",
  metadata.author = "coco ho",
  metadata.author.email = "silly2@gmail.com",
  keywords = c("hazards", "cmip6", "climate data", 'we are fkd'),
  source.author = c("Ramirez-Villegas, J.", "Stewart, P."),
  source.author.email = c("j.r.villegas@cgiar.org", "p.stewart@cgiar.org"),
  source.license = "GNUFU",
  source.citation = "Juan Hulio salsa co.",
  source.url = NULL,
  source.doi = "10010101hghghg",
  process.description = NULL,
  custom_fields = list(crop = c("rice", "maize")),
  assets = list(list(type = "video", href = "https://www.youtube.com")),
  process.derived_from = "/home/jovyan/common_data/atlas_hazards/cmip6/indices")

View(t2)



# attr_ls

# histo[[1]]
# hist(attr_ls[[1]]$hist)
# seq_along(hist)
# histo <- hist(x)
# bin_w <- (attr_ls[[1]]$hist$max - attr_ls[[1]]$hist$min) / attr_ls[[1]]$hist$num_bucket
# ylab <- seq(attr_ls[[1]]$hist$min, attr_ls[[1]]$hist$max, bin_w)
# ylab
# barplot(attr_ls[[1]]$hist$count, names = ylab[-(length(ylab))])
# length(attr_ls[[1]]$hist$count)
