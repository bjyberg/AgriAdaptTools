#' Create a bivariate data
#'
#' This function creates an raster or vector with bivariate categories
#' based on an x and y value. Paired with the bivar_legend function, this
#' function can be used alongside ggplot to create a full bivariate map as shown
#' in the below example.
#'
#' @param data A 2 layer SpatRaster or a SpatVector or sf object.
#' @param n Number of classes for the bivariate map. Suggested is 3.
#' @param x.val First attribute name (vector data only).
#' @param y.val Second attribute name (vector data only).
#' @param pal A vector of color codes to use for the map. If not provided, a
#'  default color palette is used if n = 3.
#'
#' @return A raster or vector object containing bivariate categories. If input
#'   is a raster, the output as a categorical raster where the levels are
#'   the bivariate groups. If input is a vector, the output vector contains the
#'   initial attributes plus an attribute of "bi_color" with the bivariate
#'   colors, "bi_groups" with the combined x and y categories, and 2 "*_cats"
#'   with the x and y categories.
#'
#' @examples
#' # Create a full bivariate map with legend using sf object
#' \dontrun{
#' # map_data <- make_bivariate_data(t_sf, x.val = "heat_stress", y.val = "")
#' # legend <- bivar_legend(x.title = "Heat Stress", y.title = "human_index")
#' # map <- ggplot(map_data) +
#' #   geom_sf(aes(fill = bi_color), color = NA) +
#' #   scale_fill_identity(guide = "legend") +
#' #   theme_void() +
#' #   theme(legend.position = "none")
#' # layout <- "
#' # #22
#' # 122
#' # "
#' # legend + map + plot_layout(design = layout, widths = c(.5, 1))
#' }
#'
#' @seealso [bivar_legend()] to create bivariate legend
#' @importFrom stats quantile
#' @import sf
#' @importFrom terra global classify levels concats
#' 
#' @export
make_bivariate_data <- function(data, n = 3, x.val = NULL, y.val = NULL,
    pal = NULL) {
  if (n < 3 | n > 9) {
    stop(paste0("Number of bivariate groups (n) must be between 3 and 9."))
  }
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
        "Provide a palette of", n * n, "colors."))
    }
  }
  if (any(class(data) == "SpatRaster")) {
    if (nlyr(data) == 2) {
      bivar_map <- .make_bivar_raster(data, n = n, pal = pal)[[3]]
    } else {
      stop("If using a Spat Raster, it must have 2 layers")
    }
  } else if (any(class(data) %in% c("SpatVector", "sf", "sfc"))) {
    if (is.null(x.val) | is.null(y.val)) {
      stop("Provide x and y attribute names.")
    }
    if (any(class(data) == "SpatVector")) {
      data <- st_as_sf(data)
    }
    bivar_map <- .make_bivar_vect(data, n = n, pal = pal,
      x.val = x.val, y.val = y.val)
  } else {
    stop(paste("Data must be a SpatRaster, SpatVector, or sf. Found: ",
      class(data)))
  }
  return(bivar_map)
}

.make_bivar_raster <- function(data, n = 3, categorical = FALSE, pal = NULL) {
  if (class(data) != "SpatRaster" | nlyr(data) != 2) {
    stop("Data must be a SpatRaster with 2 layers")
  }
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
        "Provide a palette of", n * n, "colors."))
    }
  }
  #   if (categorical) {
  #     n_cats <- nrow(unique(data[[1]]))
  #     if (n != n_cats) {
  #       print(
  #         "n is not equal to number of categories. Defaulted number of categories"
  #       )
  #       n <- n_cats
  #     }
  #     }
  #     if (n_cats != length(unique(data[[2]]))) {
  #       print("Categorical is TRUE, but number of categories don't match.")
  #       stop("Both Rasters must be categorical and have same number of categories.")
  #     }
  #   } else {
  quan <- global(data, quantile,
    probs = seq(0, 1, length.out = (n + 1)), na.rm = T)
  x_quans <- unname(unlist(quan[1, ]))
  data[[1]] <- classify(data[[1]], x_quans, include.lowest = TRUE)
  y_quans <- unname(unlist(quan[2, ]))
  data[[2]] <- classify(data[[2]], y_quans, include.lowest = TRUE)
  levels(data) <- rep(list(data.frame(
    ID = c(0:(n - 1)),
    group = c(1:n))), 2)
  cat_rast <- concats(data[[1]], data[[2]])
  # concats vals is 1_1, 1_2 not 1_1, 2_1 as legend, so need to transpose pal
  pal_shuffle <- t(matrix(pal, nrow = n, ncol = n))
  # levels(cat_rast) <- data.frame(ID = 0:(n*n - 1), pal = pal)
  # plot_colors <- as.vector(pal_shuffle)
  # plot(cat_rast, col = plot_colors)
  return(c(data, cat_rast))
  #   }
}

.make_bivar_vect <- function(data, n = 3, x.val, y.val, pal = NULL) {
  if (inherits(data, "sf")) {
    data_df <- as.data.frame(data)
  } else if (inherits(data, "SpatVector")) {
    data_df <- terra::as.data.frame(data, geom = "wkt")
  } else {
    stop("Data must be a SpatVector or sf object.")
  }
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
        "Provide a palette of", n * n, "colors."))
    }
  }
  if (!class(x.val) %in% c("character", "numeric") |
    !class(y.val) %in% c("character", "numeric")) {
    stop(paste0("x.val and y.val must be numeric index or",
      "character string of x/y columns in data.")
    )
  }
  pal_dict <- data.frame(
    bivar_val = paste(rep(c(1:n), n), rep(1:n, each = n), sep = "_"),
    bivar_color = pal
  )
  x_cut <- cut(
    data_df[[x.val]], # terra subset returns a date frame
    breaks = quantile(data_df[[x.val]],
      probs = seq(0, 1, length.out = (n + 1)), na.rm = T),
    include.lowest = TRUE,
    labels = 1:n
  )
  y_cut <- cut(
    data_df[[y.val]],
    breaks = quantile(data_df[[y.val]],
      probs = seq(0, 1, length.out = (n + 1)), na.rm = T),
    include.lowest = TRUE,
    labels = 1:n
  )
  data[[paste0(x.val, "_cat")]] <- x_cut
  data[[paste0(y.val, "_cat")]] <- y_cut
  data[["bivariate_groups"]] <- paste0(x_cut, "_", y_cut)
  data$bi_color <- pal_dict[match(data[["bivariate_groups"]], pal_dict[[1]]), 2]
  return(data)
}