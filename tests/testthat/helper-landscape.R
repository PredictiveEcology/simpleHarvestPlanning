library(data.table)

## A 4 x 4 landscape small enough to work out by hand. Pixels 1-8 are pixelGroup 1 (old
## Pice_mar and Betu_pap over young Abie_bal); pixels 9-16 are pixelGroup 2 (young only).
## B is g/m2 per pixel, so a landscape total is 8 x the per-pixel value of each group.
toyLandscape <- function() {
  rtm <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
                     crs = "EPSG:3978", vals = 1)
  pixelGroupMap <- terra::setValues(rtm, rep(c(1L, 2L), each = 8))
  cohortData <- data.table::data.table(
    pixelGroup     = c(1L, 1L, 1L, 2L, 2L),
    ecoregionGroup = factor("1_01"),
    speciesCode    = factor(c("Pice_mar", "Betu_pap", "Abie_bal", "Pice_mar", "Popu_tre")),
    age            = c(80L, 60L, 20L, 30L, 10L),
    B              = c(5000L, 1000L, 400L, 2000L, 500L)
  )
  list(rasterToMatch = rtm, pixelGroupMap = pixelGroupMap, cohortData = cohortData,
       thlb = rtm, planningArea = rtm)
}
