library(terra)
library(jsonlite)

# init_meta <- jsonlite::read_json("~/Biodiversity_International/mapspam_meta.json")

# json <- fromJSON("~/Biodiversity_International/mapspam_meta.json")

# json$fileGroups[json$fileGroups$name == "Spam Rasters", ]

# raster_jsons <- json$fileGroups[json$fileGroups$name == "Spam Rasters", ]

# files <- list.files(path = "/home/bjyberg/aws_bucket/MapSpam/intermediate/", pattern = "*.tif", full.names = TRUE)

# coverage_df <- data.frame(x = NA)
# coverage_df$crs <- data.frame(
#   epsg = paste0(
#     terra::crs(rast, describe = T)[c("authority", "code")],
#     collapse = ":"
#   ),
#   proj = terra::crs(rast, proj = T),
#   resolution = res(rast)[1],
#   resolutionUnit = "decimal-degrees"
# )
# coverage_df$extent <- data.frame(
#   row.names = NULL,
#   xmin = terra::ext(rast)$xmin,
#   xmax = terra::ext(rast)$xmax,
#   ymin = terra::ext(rast)$ymin,
#   ymax = terra::ext(rast)$ymax
# )
# coverage_df$temporal <- data.frame(
#   start_date = "2016/01/01",
#   end_date = "2018/12/31"
# )
# coverage_df <- coverage_df[-1]
# toJSON(coverage_df, pretty = T)




# # json$fileGroups[json$fileGroups$name == "Spam Rasters", "files"] <- toJSON(basename(files))
# # json$fileGroups[json$fileGroups$name == "Spam Rasters", "coverage"] <- toJSON(coverage_df)
# init_meta$fileGroups[[2]]$coverage <- coverage_df
# init_meta$fileGroups[[2]]$files <- basename(files)
# cat(toJSON(init_meta, pretty = T), file = "~/Biodiversity_International/mapspam_meta3.json")

# json2 <- toJSON(json, pretty = T)
# cat(json2, file = "~/Biodiversity_International/mapspam_meta2.json")
# serializeJSON(json2)
# write_json(json2, "~/Biodiversity_International/mapspam_meta2.json")

# library(aws.s3)
# aws.s3::bucket_list_df()



haxza_meta <- jsonlite::read_json("~/Biodiversity_International/hazard_metadata.json")

haxza_meta$fileGroups[[1]]$files <- list.files(
    path = "/home/bjyberg/aws_bucket/hazards/cmip6_indices_seasonal/year_timeseries/",
    pattern = "historical",
    full.names = FALSE
)
haxza_meta$fileGroups[[2]]$files <- list.files(
  path = "/home/bjyberg/aws_bucket/hazards/cmip6_indices_seasonal/year_timeseries/",
  pattern = "ssp",
  full.names = FALSE
)

r_dat <- terra::rast(
  paste0("/vsis3/digital-atlas/hazards/cmip6_indices_seasonal/year_timeseries/",
    haxza_meta$fileGroups[[2]]$files[[1]])
)

haxza_meta$fileGroups[[1]]$coverage$crs$epsg <- paste0(
  terra::crs(r_dat, describe = T)[c("authority", "code")],
  collapse = ":"
)
haxza_meta$fileGroups[[1]]$coverage$crs$proj <- terra::crs(r_dat, proj = T)
haxza_meta$fileGroups[[1]]$coverage$crs$Xresolution <- res(r_dat)[1]
haxza_meta$fileGroups[[1]]$coverage$crs$Yresolution <- res(r_dat)[2]
haxza_meta$fileGroups[[1]]$coverage$crs$resolutionUnit <- "decimal-degrees"
haxza_meta$fileGroups[[1]]$coverage$extent$xmin <- terra::ext(r_dat)$xmin
haxza_meta$fileGroups[[1]]$coverage$extent$xmax <- terra::ext(r_dat)$xmax
haxza_meta$fileGroups[[1]]$coverage$extent$ymin <- terra::ext(r_dat)$ymin
haxza_meta$fileGroups[[1]]$coverage$extent$ymax <- terra::ext(r_dat)$ymax

haxza_meta$fileGroups[[2]]$coverage$crs$epsg <- paste0(
  terra::crs(r_dat, describe = T)[c("authority", "code")],
  collapse = ":"
)
haxza_meta$fileGroups[[2]]$coverage$crs$proj <- terra::crs(r_dat, proj = T)
haxza_meta$fileGroups[[2]]$coverage$crs$Xresolution <- terra::res(r_dat)[1]
haxza_meta$fileGroups[[2]]$coverage$crs$Yresolution <- terra::res(r_dat)[2]
haxza_meta$fileGroups[[2]]$coverage$crs$resolutionUnit <- "decimal-degrees"
haxza_meta$fileGroups[[2]]$coverage$extent$xmin <- terra::ext(r_dat)$xmin
haxza_meta$fileGroups[[2]]$coverage$extent$xmax <- terra::ext(r_dat)$xmax
haxza_meta$fileGroups[[2]]$coverage$extent$ymin <- terra::ext(r_dat)$ymin
haxza_meta$fileGroups[[2]]$coverage$extent$ymax <- terra::ext(r_dat)$ymax

haxza_meta$fileGroups[[1]]$coverage$temporal$start_date <- 1995
haxza_meta$fileGroups[[1]]$coverage$temporal$end_date <- 2013
haxza_meta$fileGroups[[2]]$coverage$temporal$start_date <- 2021
haxza_meta$fileGroups[[2]]$coverage$temporal$end_date <- 2060

cat(toJSON(haxza_meta, pretty = T), file = "~/Biodiversity_International/hazard_metadata2.json")

# TODO: modular way of making json metadata
    # - l1 = Folder/metadata (SHOULD BE FOR STUFF THAT IS SIMILAR ENOUGH/SHARES SOME ATTRIBUTES)
        # - Higher level stuff
            # - Authors (data/metadata?) # groups inherit authors otherwise?
            # - description
            # - metadata date creation/update
            # - SOURCE/CITATION/SCRIPTS IF NOT GROUP SPECIFIC?
    # - l2 = list of files (could be in A sub dir) ()
        # - units (Where should these live? dir level/group level/name_convention)
        # - coverage
        # - naming conv
        # - derived from?
        # - asset links for more metadata/writeups, code, etc that is group specific