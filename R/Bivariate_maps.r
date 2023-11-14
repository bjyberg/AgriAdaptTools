#' Create a bivariate legend plot.
#'
#' This function generates a legend plot to represent bivariate data. It is a
#' grid of colored squares that can be used to represent the values of two
#' categorical variables in a bivariate plot. It is designed to be paired with
#' the `make_bivariate_data` function.
#'
#' @param x.title A character string specifying the title for the x-axis.
#' @param y.title A character string specifying the title for the y-axis.
#' @param n The number of bivariate groups (default is 3). Must be between
#'   3 and 9. A value of 3 is recommended for readability.
#' @param pal A vector of color values for the legend. If NULL, a default color
#'   palette will be used for n = 3, but you must provide a palette for n other than 3.
#' @param font_size The font size for axis titles (default is 12).
#'
#' @return A ggplot2 object representing the bivariate legend.
#'
#' @examples
#' # Generate a bivariate legend with default parameters
#' bivar_legend("X Variable", "Y Variable")
#'
#' # Create a full bivariate map with legend using sf object
#' \dontrun{
#' library(patchwork)
#' legend <- bivar_legend("X Variable", "Y Variable")
#' bivar_data <- make_bivariate_data()
#' map <- ggplot(bivar_data) +
#'  geom_sf(aes(fill = bivar_colour), color = NA) +
#'  scale_fill_identity(guide = "legend") +
#'  theme_void() +
#'  theme(legend.position = "none")
#' 
#' # create a patchwork layout
#' layout <- "
#' #22
#' 122
#' "
#' # Use patchwork to combine the map and legend
#' legend + map + plot_layout(design = layout, widths = c(.5, 1))
#' }
#' 
#' # Create a legend using n = 4
#' \dontrun{
#' pal <- c("#e8e8e8", "#bddede", "#8ed4d4", "#5ac8c8", "#dabdd4",
#'   "#bdbdd4", "#8ebdd4", "#5abdc8", "#cc92c1", "#bd92c1", "#8e92c1",
#'   "#5a92c1", "#be64ac","#bd64ac", "#8e64ac", "#5a64ac")
#'  bivar_legend("Human Heat Stress", "Minutes to Healthcare",
#'    n = 4, pal = pal)
#' }
#'
#' @seealso
#' [AAtools::make_bivariate_data()] to create bivariate data from raster or vector data
#'
#' @import ggplot2
#'
#' @export
bivar_legend <- function(x.title, y.title, n = 3, pal = NULL, font_size = 12) {
  if (n < 3 | n > 9) {
      stop(paste0("Number of bivariate groups (n) must be between 3 and 9."))
  }
  if (is.null(pal) & n == 3) {
    pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
  } else if (is.null(pal) & n != 3) {
    stop("Provide a color palette. Default palette is only allowed for n = 3.")
  }
  legend_df <- data.frame(x.var = rep(c(1:n), n),
    y.var = rep(1:n, each = n),
    fill_col = pal)
  
  legend <- ggplot(legend_df) +
    geom_tile(mapping = aes(
      x = x.var,
      y = y.var,
      fill = fill_col)) +
    scale_fill_identity() +
    labs(x = paste(x.title, "->"), #  or → for real arrow
      y = paste(y.title, "->")) +
    theme_void() +
    theme(
      axis.title = element_text(size = font_size),
      axis.title.y = element_text(angle = 90, size = font_size),
    ) +
    coord_fixed()
#   Base R way
#       legend_matrix <- matrix(c(1:(n * n)), nrow = n, ncol = n)
#       legend <- image(legend_matrix, col = pal)
#   
  return(legend)
}