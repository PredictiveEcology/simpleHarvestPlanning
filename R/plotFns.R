#Simple Harvest Plotting functions
#
#' Plot a SpatRaster object from simpleHarvest
#'
#' @param harvestRas A SpatRaster object (e.g., harvest raster)
#' @param title A character string for the plot title (optional)
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom terra as.data.frame
#' @importFrom ggplot2 ggplot aes_string geom_raster scale_fill_viridis_c coord_equal theme_bw ggtitle
plot_simpleHarvest <- function(harvestRas, title = NULL) {
  # Ensure the input is a SpatRaster
  if (!inherits(harvestRas, "SpatRaster")) {
    stop("harvestRas must be a SpatRaster object.")
  }
  
  # Convert SpatRaster to data frame for ggplot
  harvest_df <- terra::as.data.frame(harvestRas, xy = TRUE)
  
  # Determine which column to use for fill
  value_col <- names(harvest_df)[3]
  if (ncol(harvest_df) > 3) {
    warning("Multiple layers detected. Using the first layer for plotting.")
  }
  
  # Plot
  gg_harvest <- ggplot(harvest_df, aes_string(x = "x", y = "y", fill = value_col)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(na.value = "transparent") +
    theme_bw()
  
  # Add title if provided
  if (!is.null(title)) {
    gg_harvest <- gg_harvest + ggtitle(title)
  }
  
  return(gg_harvest)
}
