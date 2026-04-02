# Simple Harvest Plotting functions
#' utils::globalVariables(c("PolyID"))
#' 
#' #------------------------------------------------------
#' #' Plot Harvest polygons from simpleHarvest
#' #'
#' #' @param simpleHarvestPolys sf object with a "PolyID" column
#' #' @param title character, optional plot title
#' #'
#' #' @returns ggplot object
#' #' @export
#' #' @importFrom ggplot2 aes geom_sf ggplot scale_fill_discrete theme_bw ggtitle
#' plot_simpleHarvestPolys <- function(simpleHarvestPolys, title = NULL) {
#'   if (!is.factor(simpleHarvestPolys$PolyID)) {
#'     simpleHarvestPolys$PolyID <- as.factor(simpleHarvestPolys$PolyID)
#'   }
#'   
#'   gg_harvestp <- ggplot(simpleHarvestPolys) +
#'     geom_sf(aes(fill = PolyID)) +
#'     scale_fill_discrete() + 
#'     theme_bw()
#'   
#'   if (!is.null(title)) {
#'     gg_harvestp <- gg_harvestp + ggtitle(title)
#'   }
#'   return(gg_harvestp)
#' }

#' #------------------------------------------------------
#' #' Plot a SpatRaster object from simpleHarvest
#' #'
#' #' @param x A SpatRaster object
#' #' @param title A character string for the plot title 
#' #'
#' #' @return ggplot object
#' #' @export
#' #' @importFrom ggplot2 ggplot aes_string scale_fill_viridis_c coord_equal theme_bw ggtitle
#' #' @importFrom tidyterra geom_spatraster
#' plot_simpleHarvest <- function(x, title = NULL) {
#'   if (!inherits(x, "SpatRaster")) {
#'     stop("x must be a SpatRaster object.")
#'   }
#'   
#'   gg_harvest <- ggplot() +
#'     geom_spatraster(data = x) +
#'     coord_equal() +
#'     scale_fill_viridis_c(na.value = "transparent") +
#'     theme_bw()
#'   
#'   if (!is.null(title)) {
#'     gg_harvest <- gg_harvest + ggtitle(title)
#'   }
#'   return(gg_harvest)
#' }

#------------------------------------------------------
#' #' Plot age map from simpleHarvest
#' #'
#' #' @param x SpatRaster corresponding to stand age or time since disturbance
#' #' @param title character, plot title
#' #' @param maxAge numeric, maximum age to plot
#' #'
#' #' @returns ggplot object
#' #' @export
#' #' @importFrom ggplot2 ggplot ggtitle scale_fill_viridis_c coord_equal theme_bw
#' #' @importFrom tidyterra geom_spatraster
#' plot_simpleHarvestageMap <- function(x, title = NULL, maxAge) {
#'   if (!inherits(x, "SpatRaster")) {
#'     stop("x must be a SpatRaster object.")
#'   }
#'   
#'   x[x > maxAge] <- maxAge
#'   
#'   gg_agemap <- ggplot() +
#'     geom_spatraster(data = x) +
#'     coord_equal() +
#'     scale_fill_viridis_c(na.value = "transparent") +
#'     theme_bw()
#'   
#'   if (!is.null(title)) {
#'     gg_agemap <- gg_agemap + ggtitle(title)
#'   }
#'   return(gg_agemap)
#' }

#------------------------------------------------------
#' Plot Harvest maps
#'
#' @param x SpatRaster for current or cumulative harvest
#' @param title character, the plot title
#' @param subtitle character, the plot subtitle
#'
#' @returns ggplot object
#' @export
#' @importFrom ggplot2 ggplot ggtitle theme_bw labs
#' @importFrom tidyterra geom_spatraster
#' @importFrom viridis scale_fill_viridis
plot_harvestMap <- function(x, title = NULL, subtitle = NULL) {
  if (!inherits(x, "SpatRaster")) {
    stop("x must be a SpatRaster object.")
  }
  
  gg_hm <- ggplot() +
    geom_spatraster(data = x) +
    scale_fill_viridis(na.value = "transparent") +
    theme_bw()
  
  if (!is.null(title) || !is.null(subtitle)) {
    gg_hm <- gg_hm + ggtitle(title) + labs(subtitle = subtitle)
  }
  
  return(gg_hm)
}
