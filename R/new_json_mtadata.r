library(terra)
collection_metadata <- function(
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
  assets = NULL # asset format function and dictionary function
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
    collection.metadata$path <- collection.path
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

itemgroup_metadata <- function( # TODO: add write fn to this and catalog
  collection,
  itemgroup.list, # NOTE: File List should be relative to top-level json folder metadata
  itemgroup.name,
  itemgroup.description, #What is it /how is it different from the other groups?
  layers_fields, # should be the same across groups
  author.name,
  author.email,
  itemgroup.unit,
  file.type,
  name.format,
  name.separator = "_",
  coverage.region,
  temporal.resolution,
  temporal.start_date,
  temporal.end_date,
  source.license = NULL,
  source.citation = NULL,
  source.url = NULL,
  source.doi = NULL,
  process.derived_from = NULL,
  process.description = NULL,
  process.code = NULL,
  custom_fields = NULL,
  assets = NULL,
  item.lyr.histograms = FALSE,
  item.lyr.Names = FALSE,
  item.lyr.stats = FALSE
){
    if (inherits(collection, "character")) {
      if (!file.exists(collection)) {
      stop(paste0(
        "Folder metadata must be a valid path to an existing",
        " `_digiAtlas-metadata.json`",
        " file, or a list object made by the collection function."
      ))
      }
      collection <- jsonlite::fromJSON(collection)
    }
    if (!inherits(collection, "list")) {
      stop(paste0(
        "Folder metadata must be a valid path to an existing",
        " `_digiAtlas-metadata.json`",
        " file, or a list object made by the collection function."
      ))
    }
    if (itemgroup.name %in% collection[["fileGroups"]][["name"]]) {
      stop("Item group name already exists in folder metadata")
    }
    base_folder <- collection$path
    collection$metadata$dateModified <- format(Sys.time(), "%Y-%m-%d")
    author <- .author(author.name, author.email)
    spatial <- .spatial_coverage(itemgroup.list[1], coverage.region)
    temporal <- .temporal_coverage(temporal.resolution, temporal.start_date, temporal.end_date)
    coverage <- c(spatial, temporal)
    source <- .source(source.license, source.citation, source.url, source.doi)
    files <- .files(itemgroup.list, base_folder)
    nameScheme <- list(nameFormat = name.format, separator = name.separator)
    files_attrs <- list()
    if (item.lyr.Names|item.lyr.stats|item.lyr.histograms) {
      tryCatch(
        {
          for (i in seq_along(files)) {
            f_name <- files[[i]]
            position = grep(f_name, itemgroup.list)
            layer_attributes <- .layers(itemgroup.list[[position]],
            item.lyr.stats, item.lyr.histograms)
            files_attrs[[f_name]] <- layer_attributes
          }
        },
        error = function(e) {
          print(paste0("Failed to read layers from ", itemgroup.list[i]))
        })
    } else {
      files_attrs <- files
    }

    group_metadata <- list(
    name = itemgroup.name,
    description = itemgroup.description,
    author = author,
    layers = layers_fields,
    fileType = file.type,
    source = source,
    coverage = coverage,
    nameScheme = nameScheme,
    process = list(
      derived_from = process.derived_from,
      description = process.description,
      code = process.code
    ),
    other_metadata = custom_fields,
    assets = assets,
    files = files_attrs
  )

  collection[["fileGroups"]] <- append(collection[["fileGroups"]],
    setNames(list(hold = group_metadata), itemgroup.name))
  
  out_nam <- paste0(collection$path, "/", collection$ID,
    "_digiAtlas-metadata.json") # THIS PART OF NAME HARDCODED TO FIND LATER
  jsonlite::toJSON(collection, pretty = T, auto_unbox = T) |>
    cat(file = out_nam)

  return(collection)
}

# Helper functions
.files <- function(file_list, base_path) {
  rel_file <- gsub(paste0(".*", base_path), "", file_list)
  clean_rel_file <- sub("^/|^//", "", rel_file)
  files <- clean_rel_file[clean_rel_file != ""]
  return(files)
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
  if (stats) {
    if (inherits(x, "SpatRaster")) {
      stats_df <- terra::global(x, c("mean", "sd", "min", "max"), na.rm = T)
      stats_df$dtype <- terra::datatype(x)
      stats_df$name <- row.names(stats_df)
      row.names(stats_df) <- NULL
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
      stats_df <- as.data.frame(do.call(rbind, df_list))
      names(stats_df) <- c("mean", "sd", "min", "max", "name", "dtype")
    } else {
      stop(paste(type, "is currently unsupported. No data will be added."))
    }
    for (i in names(x)) {
      attr_ls[[i]]$stats <- as.list(stats_df[stats_df$name == i, ])
    }
  }
  if (hist) {
    histos <- list()
    if (inherits(x, "data.frame")) {
      hist_cols <- names(x)[sapply(x, is.numeric)]
      for (i in hist_cols) {
        histos[[i]] <- hist(x[[i]], plot = FALSE)
      }
    } else if (inherits(x, "SpatRaster")) {
      for (i in terra::nlyr(x)) {
        histos[[i]] <- hist(x[[i]], plot = FALSE)
      }
    }
    for (i in seq_along(histos)) {
      name <- histos[[i]]$xname
      attr_ls[[name]]$hist$num_bucket <- length(histos[[i]]$counts) # num buckets
      attr_ls[[name]]$hist$min <- min(histos[[i]]$breaks)
      attr_ls[[name]]$hist$max <- max(histos[[i]]$breaks)
      attr_ls[[name]]$hist$count <- histos[[i]]$counts
    }
  }
  return(attr_ls)
}



conflict_collection <- collection_metadata(
  collection.ID = "atlas_conflict0",
  collection.path = "/home/bjyberg/Biodiversity_International/Adaptation_Atlas/Conflict",
  collection.description = "Conflcit data for the adaptation atlas",
  metadata.author = "Brayden Youngberg",
  metadata.author.email = "bjyberg1@gmail.com",
  keywords = list("adaptive capaticy","vulnerability", "acled", 'conflict'),
  source.author = c("Youngberg, B.", "Vyas, S."),
  source.author.email = c("bjyberg1@gmail.com", "s.vyas@cgiar.org"),
  source.license = NULL,
  source.citation = NULL,
  source.url = NULL,
  source.doi = NULL,
  process.description = NULL,
  process.derived_from = NULL,
  process.code = NULL)

  acled_itemgroup0 <- itemgroup_metadata(
    collection = conflict_collection,
    itemgroup.list = "/home/bjyberg/Biodiversity_International/Adaptation_Atlas/Conflict/conflict_historical_2020-2023.tif",
    itemgroup.name = "acled_raw",
    itemgroup.description = "Rasterized ACLED data",
    itemgroup.unit = "# conflicts per pixel",
    layers_fields = NULL,
    author.name = "Brayden Youngberg",
    author.email = "bjyberg1@gmail.com",
    file.type = "COG",
    name.format = "acled_raw",
    name.separator = "_",
    coverage.region = "SSA",
    temporal.resolution = "NA",
    temporal.start_date = "2020-04-26",
    temporal.end_date = "2023-04-32",
    source.license = NULL,
    source.citation = NULL,
    source.url = NULL,
    source.doi = NULL,
    assets = list(type = "documentation", path = 'Conflict Notes.docx'),
    item.lyr.histograms = TRUE,
    item.lyr.stats = TRUE,
    item.lyr.Names = TRUE,
    process.description ="A rasterized, normalized and aggregated version of the ACLED conflict .csv file with agreements and peaceful protests removed. It was processed using kernel density estimate to indicate the effect of conflict.",
    process.derived_from = "ACLED_2020-04-26-2023-04-23_Africa.csv",
    process.code = "conflict_layer-builder.r"
  )
