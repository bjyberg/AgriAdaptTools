#' Generate meta-data for Adaptation Atlas Layers
#'
#' This function will assist you to systematically generate meta-data for the Adaptation Atlas.
#'
#' The `data` parameter can be used to supply a Spatraster, SpatVector, Stars, or sf object from which geospatial dataset will automatically extracted.
#' The `data` parameter can also be a file path to a spatial file such as .gpkg, .tif, .nc4, etc.
#' In all cases provide multiple values as a list, for example `list(10.1038/nature25181, 10.1038/EE23432)`
#'
#' @param data A `SpatRaster` or `SparVector` object.
#' @param folder The directory path for where the output metadata should be saved.
#' @param save_output If folder is provided, should the metadata be saved as a csv or txt file?
#' @param dataset.title_short A short title to label the dataset. e.g., `MapSPAM`.
#' @param dataset.title_long A full title to label the dataset. e.g., `Travel time to cities`.
#' @param dataset.desc A short description enabling the user to quickly understand the subject of the dataset and how to interpret it. For example, `The value of each pixel is the estimated travel time in minutes to the nearest urban area in 2015. There are 12 data layers based on different sets of urban areas, defined by their population in year 2015`.
#' @param dataset.author Name of authors. Surname of lead author only.
#' @param dataset.contact If the dataset has a gate-keeper or contact author list their full name here.
#' @param dataset.contact_email If the dataset has a gate-keeper or main author list their email address here.
#' @param dataset.pub_doi The doi for the publication(s) associated with this datasest. Use a list to provide multiple entries. For example, `list("10.1038/nature25181","10.1038/EE23432")`
#' @param dataset.data_doi The doi for this dataset. This could be a Harvard dataverse doi, for example, `10.7910/DVN/FSSKBW`.
#' @param dataset.sourceurl The link that gets the user closest to the dataset if this is not the dataset doi. For example, a page describing a dataset API or data portal such as `https://www.fao.org/faostat/en/#data/QCL`. If there is a github associated with analysis please list this in the methods section.
#' @param dataset.projecturl If the dataset has a project website then record the url here, for example `https://www.mapspam.info/`.
#' @param dataset.citation Main citation(s) for this dataset in Havard format. Use a list to separate multiple entries.
#' @param dataset.licence Is the dataset open-access? The information to is to include the licence type, other wise `open` or `private` will suffice.
#' @param file.filename The filename of the dataset `e.g., aez_global.shp`.
#' @param file.format The format of the file, for example `LZW compressed GeoTIFF, shapefile, netcdf4`.
#' @param file.data_type Type and/or size of the dataset, for example  `Byte (16 bit Unsigned Integer)`.
#' @param file.no_value_data The value that indicates no-data or NA values in the dataset, for example `65535` or `9999`.
#' @param file.file_naming_convention A free text description of any file naming convention that applies, so that a user can easily interpret the filename.
#' @param file.flags Additional flags for the file which controls how the file is opened.
#' @param variable.theme One of `hazard`, `exposure`, `adaptative_capacity`, `solutions`, `enabling environment` or combinations of these. Use a `.` delimiter to indicate a combination for example, `hazard.exposure`.
#' @param variable.subtheme Please use as few words as possible to describe a subtheme, for example `rainfall`, `heat stress`, or `access to markets`.
#' @param variable.name The specific name of the variable, for example `crop yield`, `biophysical suitability` or `aridity index`. If the dataset refers to a specific crop the crop name should be recorded in the `variable.commodity` parameter.
#' @param variable.subname If more groupings are needed to describe the data structure then this column can be used. For example MapSPAM value of production data could have  `variable.name=value of production` and  `variable.subname = all technologies` or `variable.subname = rainfed only`. Do not record information about statistics here, use the `variable.statistic` parameter instead.
#' @param variable.commodity If the variable relates to a specific crop please include the crop name here (scientific name preferred). For example, `Zea mays`
#' @param variable.type The type of variable, usually one of `continuous`, `categorical`, or `ordinal`. For ordinal or categorical rasters please complete the `data.categorical_val` and `data.categorical_desc` parameters.
#' @param variable.statistic If a variable has different files that represent different calculations or groupings, such as the mean and confidence intervals of a variable, then please note the calculation here. For example, `mean`, `median`, `95% CI high`,`95% CI low`, etc.
#' @param variable.unit Unit of measurement, for example `kg/ha`, `kg/kg`, `days`, `minutes`, etc. See `https://www.ebi.ac.uk/ols/ontologies/om` for standardized SI units. Please use a forward slash `/` delim rather than `-1` (so `kg/ha` not `kg ha-1`).
#' @param method.analysis_type A general description of the type of analysis employed. For example, `niche model` or`DSSAT crop model`.
#' @param method.description A summary of methods used to create the dataset.
#' @param method.github If there is a github repo associated with the analysis please list it here. Ideally it should walk a user though the steps to recreate a datasest.
#' @param method.qual_indicator The name of a data avaiable indicator, typically for niche or analogue layers derived from empirical data. For example `number of studies`.
#' @param method.qual_availability The value for the data availability indicator defined in the row above. This can be numeric or ordinal (e.g. `high`, `medium`, or `low`)
#' @param grid.proj This field is auto-filled if the dataset is provided as the `data` parameter. The proj4 string of the dataset.
#' @param grid.epsg This field is auto-filled if the dataset is provided as the `data` parameter. The EPSG code of the dataset.
#' @param grid.yres This field is auto-filled if the dataset is provided as the `data` parameter. For gridded datasets, the spatial resolution of celll longitude dataset in decimal degrees.
#' @param grid.xres This field is auto-filled if the dataset is provided as the `data` parameter. For gridded datasets, the spatial resolution of celll latitude dataset in decimal degrees.
#' @param grid.xmin This field is auto-filled if the dataset is provided as the `data` parameter. Minimum latitude of dataset in decimal degrees.
#' @param grid.xmax This field is auto-filled if the dataset is provided as the `data` parameter. Maximum latitude of dataset in decimal degrees.
#' @param grid.ymin This field is auto-filled if the dataset is provided as the `data` parameter. Minimum longtiude of dataset in decimal degrees.
#' @param grid.ymax This field is auto-filled if the dataset is provided as the `data` parameter. Maximum longtiude of dataset in decimal degrees.
#' @param grid.nrow This field is auto-filled if the dataset is provided as the `data` parameter. Number of rows in gridded data.
#' @param grid.ncol This field is auto-filled if the dataset is provided as the `data` parameter. Number of cols in gridded data.
#' @param temporal.resolution What time period does the dataset refer to? For example `annual`, `dekadal`, `daily`, `long-term average` or similar.
#' @param temporal.start_date The start date of the temporal period for which the dataset is revelant to or derived from. Preferably a date as `yyyy--mm-dd`, `yyyy-mm` or `yyyy`. For example, `2018-01-01`, `2018-01` or `2018`. Please use r date format `yyyy-mm-dd`.
#' @param temporal.end_date The end date of the temporal period for which the dataset is revelant to or derived from. Preferably a date as `yyyy--mm-dd`, `yyyy-mm` or `yyyy`. For example, `2018-01-01`, `2018-01` or `2018`. Please use r date format `yyyy-mm-dd`.
#' @param data.categorical_val For gridded categorical or ordinal datasets provide a list of values that correspond to values in the dataset, e.g. `list(1,2,3,4,5)`.
#' @param data.categorical_desc For the values in `data.categorical_val` provide a list of descriptions that correspond to these values, e.g. `list("none","mild","moderate","severe","extreme")`.
#' @param data.shapefile_field For the data.table associated with a shapefile please list the names of key fields, e.g. `list("Admin0","Admin1","% of population")`.
#' @param data.shapefile_type For the fields listed in `data.shapefile_field` please provide a list of data types, e.g. `list("categorical","categorical","continuous")`.
#' @param data.shapefile_unit For the fields listed in `data.shapefile_field` please provide a list of units, e.g. `list(NA,NA,"proportion")`.
#' @param data.shapefile_description For the fields listed in `data.shapefile_field` please provide a description of the field, e.g. `list("Country name","Administrative unit 1 name","Poverty headcount ratio at national poverty lines")`.
#' @return A data.frame of meta.data
#' @importFrom sf st_crs st_bbox
#' @importFrom stars st_res
#' @importFrom terra crs ext nrow ncol res
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils write.csv
#'
#' @export
atlas_metadata <- function(data = NA,
                           folder = NA,
                           save_output = c("csv", "txt"),
                           dataset.title_short = "", # make Required
                           dataset.title_long = "",
                           dataset.desc = "", # make Required
                           dataset.author = "", # make Required
                           dataset.contact = "", # make Required
                           dataset.contact_email = "", # make Required
                           dataset.pub_doi = "",
                           dataset.data_doi = "",
                           dataset.sourceurl = "",
                           dataset.projecturl = "",
                           dataset.citation = "",
                           dataset.licence = "",
                           file.filename = "",
                           file.format = "",
                           file.data_type = "", # make Required
                           file.no_value_data = "",
                           file.file_naming_convention = "",
                           file.flags = "",
                           variable.theme = "",
                           variable.subtheme = "",
                           variable.name = "",
                           variable.subname = "",
                           variable.commodity = "",
                           variable.type = "",
                           variable.statistic = "",
                           variable.unit = "", # make Required
                           method.analysis_type = "",
                           method.description = "",
                           method.github = "",
                           method.qual_indicator = "",
                           method.qual_availability = "",
                           grid.proj = "",
                           grid.epsg = "",
                           grid.xres = "",
                           grid.yres = "",
                           grid.xmin = "",
                           grid.xmax = "",
                           grid.ymin = "",
                           grid.ymax = "",
                           grid.nrow = "",
                           grid.ncol = "",
                           temporal.resolution = "",
                           temporal.start_date = "",
                           temporal.end_date = "",
                           data.categorical_val = "",
                           data.categorical_desc = "",
                           data.shapefile_field = "",
                           data.shapefile_type = "",
                           data.shapefile_unit = "",
                           data.shapefile_description = ""
) {
  match.arg(save_output)
  metadata.date <- Sys.Date()
  if (inherits(data, "character")) {
    ext <- tools::file_ext(data)
    folder <- dirname(data)
    if (file.filename == "") {
      file.filename <- basename(data)
    }
    if (ext %in% c("tif", "tiff", "asc", "nc4", "nc")) {
      data <- terra::rast(data)
    } else if (ext %in% c("shp", "gpkg", "geojson")) {
      data <- terra::vect(data)
    } else {
      stop("If file path is provided, it must be a raster or vector file")
    }
  }

  # Add geospatial metadata from terra, stars, or sf objects
  if (any(class(data) %in% c("stars", "sf", "sfc"))) {
    grid.proj <- sf::st_crs(data)$proj4string
    grid.epsg <- paste0("epsg:", sf::st_crs(data)$epsg)
    grid.xmin <- sf::st_bbox(data)$xmin
    grid.xmax <- sf::st_bbox(data)$xmax
    grid.ymin <- sf::st_bbox(data)$ymin
    grid.ymax <- sf::st_bbox(data)$ymax
    if (inherits(data, "stars")) {
      grid.xres <- stars::st_res(data)[1]
      grid.yres <- stars::st_res(data)[2]
      grid.nrow <- nrow(data)
      grid.ncol <- ncol(data)
    }
  } else if (class(data) %in% c("SpatRaster", "SpatVector")) {
    grid.proj <- terra::crs(data, proj = T)
    grid.epsg <- paste0(
      terra::crs(data, describe = T)[c("authority", "code")],
      collapse = ":"
    )
    grid.xmin <- terra::ext(data)$xmin
    grid.xmax <- terra::ext(data)$xmax
    grid.ymin <- terra::ext(data)$ymin
    grid.ymax <- terra::ext(data)$ymax
    if (inherits(data,"SpatRaster")) {
      grid.xres <- terra::res(data)[1]
      grid.yres <- terra::res(data)[2]
      grid.nrow <- terra::nrow(data)
      grid.ncol <- terra::ncol(data)
    }
  } else {
    print(paste("Data is not a SpatRaster, SpatVector, Stars, or sf object.",
      "No spatial metadata will be extracted"))
  }

  Meta.data <- data.frame(
    metadata.date, dataset.title_short, dataset.title_long, dataset.desc,
    dataset.author, dataset.contact, dataset.contact_email,
    dataset.pub_doi, dataset.data_doi, dataset.sourceurl,
    dataset.projecturl, dataset.citation, dataset.licence,
    file.filename, file.format, file.data_type, file.no_value_data,
    file.file_naming_convention,  file.flags, variable.theme, variable.subtheme,
    variable.name, variable.subname, variable.commodity, variable.type,
    variable.statistic, variable.unit, method.analysis_type, method.description,
    method.github, method.qual_indicator, method.qual_availability, grid.proj,
    grid.epsg, grid.xres, grid.yres, grid.xmin, grid.xmax, grid.ymin, grid.ymax,
    grid.nrow, grid.ncol, temporal.resolution, temporal.start_date,
    temporal.end_date, data.categorical_val, data.categorical_desc,
    data.shapefile_field, data.shapefile_type, data.shapefile_unit,
    data.shapefile_description
  )

  File <- tools::file_path_sans_ext(file.filename)

  if (!is.na(folder)) {
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = T)
    }
    print(paste("Writing metadata for", File, "to folder", folder))
    if (save_output == "csv") {
      utils::write.csv(Meta.data, file = paste0(folder, "/", File, "_metadata.csv"))
    } else if (save_output == "txt") {
      text <- character(length(Meta.data))
      for (i in 1:length(Meta.data)) {
        text[i] <- paste(names(Meta.data)[i], Meta.data[[i]], sep = ": ")
      }
      writeLines(text, paste0(folder, "/", File, "_metadata.txt"))
    }
  }
  return(Meta.data)
}