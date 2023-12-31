% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Make_bivar_data.r
\name{make_bivariate_data}
\alias{make_bivariate_data}
\title{Create a bivariate data}
\usage{
make_bivariate_data(
  data,
  n = 3,
  x.val = NULL,
  y.val = NULL,
  pal = NULL,
  categorical = FALSE
)
}
\arguments{
\item{data}{A 2 layer SpatRaster or a SpatVector or sf object.}

\item{n}{Number of classes for the bivariate map. Suggested is 3.}

\item{x.val}{First attribute name (vector data only).}

\item{y.val}{Second attribute name (vector data only).}

\item{pal}{A vector of color codes to use for the map. If not provided, a
default color palette is used if n = 3.}

\item{categorical}{An optional logical value for use if data is a categorical SpatRaster}
}
\value{
A raster or vector object containing bivariate categories. If input
is a raster, the output as a categorical raster where the levels are
the bivariate groups. If input is a vector, the output vector contains the
initial attributes plus an attribute of "bi_color" with the bivariate
colors, "bi_groups" with the combined x and y categories, and 2 "*_cats"
with the x and y categories.
}
\description{
This function creates an raster or vector with bivariate categories
based on an x and y value. Paired with the bivar_legend function, this
function can be used alongside ggplot to create a full bivariate map as shown
in the below example.
}
\examples{
# Create a full bivariate map with legend using sf object
\dontrun{
map_data <- make_bivariate_data(t_sf, x.val = "heat_stress", y.val = "")
legend <- bivar_legend(x.title = "Heat Stress", y.title = "human_index")
map <- ggplot(map_data) +
  geom_sf(aes(fill = bi_color), color = NA) +
  scale_fill_identity(guide = "legend") +
  theme_void() +
  theme(legend.position = "none")
layout <- "
#22
122
"
legend + map + plot_layout(design = layout, widths = c(.5, 1))
}
# Create a bivariate map with 4 groups and a categorical raster
\dontrun{
heat_stress <- rast(/path/to/heat_stress) # a categorical raster of 4 groups
healthcare <- rast(/path/to/healthcare) # initially a non-categorical raster

#' pal <- c("#e8e8e8", "#bddede", "#8ed4d4", "#5ac8c8", "#dabdd4",
  "#bdbdd4", "#8ebdd4", "#5abdc8", "#cc92c1", "#bd92c1", "#8e92c1",
  "#5a92c1", "#be64ac","#bd64ac", "#8e64ac", "#5a64ac")
# the healthcare raster needs to be made categorical same # of levels as the heat stress
health_levels <- classify(healthcare, c(30, 60, 120, 240, 6921), include.lowest = T)
levels(health_levels) <- c("Low", "Medium", "High", "Very High")
health_levels <- as.factor(health_levels)  # make sure both are read as factors
vars <- c(as.factor(heat_stress), health_levels)
make_bivariate_data(vars, n = 4, categorical = TRUE, pal = pal)
}

}
\seealso{
\code{\link[=bivar_legend]{bivar_legend()}} to create bivariate legend
}
