
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "simpleHarvest",
  description = paste("This is a very simplistic harvest module designed to interface with the LandR suite of modules",
                      "It will create a raster of harvested patches, but will not simulate actual harvest.",
                      "Should be paired with LandR_reforestation"),
  keywords = c("harvest", "LandR", "rstCurrentHarvest"),
  authors = c(
     person(c("Ian"), "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
     person("Parvin", "Kalantari", email = "parvin.kalantari@nrcan-rncan.gc.ca", role = "ctb")
     ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9008", simpleHarvest = "0.0.1"),
  #spatialExtent = raster::extent(rep(NA_real_, 4)),
  spatialExtent = terra::ext(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "simpleHarvest.Rmd"),
  reqdPkgs = list("PredictiveEcology/LandR", "sp", "raster", 'sf', 'magrittr', 'fasterize'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?
                    This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("harvestTarget", "numeric", 0.01, 0, 1,
                    desc= "proportion of harvestable area to harvest each timestep"),
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA, desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 0.24, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 0.24 will likely result in harvest patches that are maximum size"))
  ),
  inputObjects = bind_rows(
    expectsInput('cohortData', objectClass = 'data.table', desc = "table with pixelGroup, age, species, and biomass of cohorts"),
    expectsInput(objectName = 'demRaster', objectClass = 'RasterLayer', desc = 'a DEM used to exclude areas from harvest.'),
    expectsInput("pixelGroupMap", objectClass = "RasterLayer", desc = "Raster giving locations of pixelGroups"),
    expectsInput('rasterToMatch', objectClass = 'RasterLayer',
                 desc = 'a template raster for all raster operations. Cannot have lat/long projection'),
    expectsInput("studyArea", objectClass = "SpatialPolygonsDataFrame", desc = "study area used to crop spatial inputs, if applicable",
                 sourceURL = "https://drive.google.com/open?id=1TlBfGfes_6UQW4M3jib8zgY5sGd_yjmY"),
    expectsInput("thlb", objectClass = "RasterLayer",
                 desc = paste("binary raster with 1 denoting harvestable pixels. Must match spatial attributes of rasterToMatch.",
                              "Pixels that are NA in pixelGroupMap but non-NA in thlb will be coerced to NA."))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = 'rstCurrentHarvest', objectClass = 'RasterLayer',
                  desc = 'Binary raster representing annual harvested areas'),
    createsOutput(objectName = "cumulativeHarvestMap", objectClass = "RasterLayer",
                  desc = "cumulative harvest in raster form"),
    createsOutput(objectName = "harvestSummary", objectClass = "data.table",
                  desc = "data.table with year and pixel index of harvested pixels")
  )
))


doEvent.simpleHarvest = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "simpleHarvest", "harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "simpleHarvest", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "simpleHarvest", "save")

    },
    plot = {
      plot(sim$rstCurrentHarvest)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "simpleHarvest", "plot")

    },
    save = {

      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "simpleHarvest", "save")

    },
    harvest = {

      sim$rstCurrentHarvest <- harvestSpreadInputs(pixelGroupMap = sim$pixelGroupMap,
                                                   cohortData = sim$cohortData,
                                                   thlb = sim$thlb,
                                                   spreadProb = P(sim)$spreadProb,
                                                   maxCutSize = P(sim)$maxPatchSizetoHarvest,
                                                   target = P(sim)$harvestTarget,
                                                   minAgesToHarvest = P(sim)$minAgesToHarvest)
      sim$cumulativeHarvestMap <- sim$rstCurrentHarvest + sim$cumulativeHarvestMap
      harvestIndex <- data.table(year = time(sim), pixelIndex = 1:ncell(sim$rstCurrentHarvest),
                                 harvestStat = getValues(sim$rstCurrentHarvest))
      harvestIndex <- harvestIndex[harvestStat == 1,]
      harvestIndex[, harvestStat := NULL]

      sim$harvestSummary <- rbind(sim$harvestSummary, harvestIndex)
      sim$rstCurrentHarvest@data@attributes$Year <- time(sim)

      sim <- scheduleEvent(sim, time(sim) + 1,  "simpleHarvest", "harvest")

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  #initial harvest raster is 0 everywhere
  #sim$cumulativeHarvestMap <- setValues(sim$rasterToMatch, 0)
  sim$cumulativeHarvestMap <- terra::values(sim$rasterToMatch, 0)
  sim$harvestSummary = terra::data.table(year = integer(0), pixelIndex = integer(0))
  return(invisible(sim))
}


### template for save events
Save <- function(sim) {

  sim <- saveFiles(sim)

  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {

  plot(sim$rstCurrentHarvest)

  return(invisible(sim))
}

harvestSpreadInputs <- function(pixelGroupMap,
                                cohortData,
                                thlb,
                                spreadProb,
                                maxCutSize,
                                minAgesToHarvest,
                                target) {

  #thlb[is.na(getValues(pixelGroupMap))] <- NA #pixels not in pixelGroupMap cannot be harvested
  # use terra function in stead of raster function getValues ()
  # vals <- values(thlb)
  # vals[is.na(values(pixelGroupMap))] <- NA
  # values(thlb) <- vals
  
  thlb <- terra::mask(thlb, pixelGroupMap) # Keeps values in thlb where pixelGroupMap is not NA
  
  #Make an ageMap
  cohortData <- copy(cohortData)
  standAges <- cohortData[, .(BweightedAge = sum(B * age)/sum(B)), .(pixelGroup)]

  pixID <- data.table('pixelGroup' = getValues(pixelGroupMap),
                      "pixelIndex" = 1:ncell(pixelGroupMap),
                      "thlb" = getValues(thlb))
  pixID <- na.omit(pixID) #drop non-forested pixels, even if they're in thlb
  pixID <- pixID[thlb == 1,] #drop pixels that aren't in thlb

  landStats <- standAges[pixID, on = c("pixelGroup")]
  landStats <- landStats[BweightedAge >= minAgesToHarvest,]
  #I assume this method is faster than matching the pixelGroup raster values with a vector
  #harvestableAreas <- raster(thlb)
  harvestableAreas <- terra::rast(thlb)
  harvestableAreas[landStats$pixelIndex] <- spreadProb

  #calculate harvest target
  harvestTarget <- round(nrow(landStats) * target)
  minCuts <- round(harvestTarget/maxCutSize)

  #calculate initial cuts by assuming every cut reaches the max
  initialCuts <- sample(landStats$pixelIndex, size = minCuts, replace = FALSE)

  iteration <- spread2(landscape = harvestableAreas,
                       start = initialCuts,
                       asRaster = FALSE,
                       spreadProb = harvestableAreas,
                       maxSize = maxCutSize)

  #rstCurrentHarvest <- raster(pixelGroupMap)
  rstCurrentHarvest <- terra::rast(pixelGroupMap)
  #set harvestable areas as 0
  rstCurrentHarvest[!is.na(pixelGroupMap[])] <- 0

  #update the current harvest and available areas
  rstCurrentHarvest[iteration$pixels] <- 1
  harvestableAreas[iteration$pixels] <- NA

  totalCut <- nrow(iteration)
  #iterate through spread until harvest is sufficient
  #0.97 creates a buffer so harvest is not guaranteed to exceed target rate
  while (totalCut <= 0.97 * harvestTarget) {

    newCuts <- round(c(1 - totalCut/harvestTarget) * minCuts)
    newLocs <- landStats[!pixelIndex %in% iteration$pixels]$pixelIndex
    newCutLocs <- sample(newLocs, size = newCuts, replace = FALSE)

    nextIteration <- spread2(landscape = harvestableAreas,
                             start = newCutLocs,
                             asRaster = FALSE,
                             spreadProb = harvestableAreas,
                             maxSize = maxCutSize)

    harvestableAreas[nextIteration$pixels] <- 0
    rstCurrentHarvest[nextIteration$pixels] <- 1

    totalCut <- totalCut + nrow(nextIteration)
    minCuts <- c(minCuts + newCuts)
  }

  return(rstCurrentHarvest)
}

.inputObjects <- function(sim) {

  if (!all(unlist(lapply(c("pixelGroupMap", "cohortData", "rasterToMatch", "thlb"), suppliedElsewhere, sim = sim)))) {
    stop("please supply all objects - this module has no defaults at this time")
  }
  return(sim)
}
