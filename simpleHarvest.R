
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
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "simpleHarvest.Rmd"),
  reqdPkgs = list("PredictiveEcology/LandR@development (>= 1.1.5.9055)", 'sf', 'magrittr', 'fasterize', "terra"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This is here for backwards compatibility. Please use `.plots`"),
    defineParameter(".plots", "character", "png", NA, NA,
                    "This describes the type of 'plotting' to do. See `?Plots` for possible types. To omit, set to NA"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,"This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?
                    This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("startTime", "numeric", start(sim), NA, NA,
                    desc = "Simulation time at which to initiate harvesting"),
    defineParameter("harvestTarget", "numeric", 0.01, 0, 1,
                    desc= "proportion of harvestable area to harvest each timestep"),
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA, desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 0.24, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 0.24 will likely result in harvest patches that are maximum size"))
  ),
  
  # Modified for terra: 
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = "table with pixelGroup, age, species, and biomass of cohorts"),
    expectsInput(objectName = "pixelGroupMap", objectClass = "SpatRaster", desc = "Raster giving locations of pixelGroups"),   
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster", desc = "Template raster"),                         
    expectsInput(objectName = "studyArea", objectClass = "SpatVector", desc = "Study area polygon",                            
                 expectsInput(objectName = "thlb", objectClass = "SpatRaster", desc = "Harvestable pixels mask"),                           
                 expectsInput(objectName ="timeSinceHarvest", objectClass = "SpatRaster", desc = "map of time since last harvest - with  harvest start pixels = 0"))
  ),
  
  # Modified for terra: objectClass = 'RasterLayer' to objectClass ="SpatRaster"
  outputObjects = bind_rows(
    createsOutput(objectName = "rstCurrentHarvest", objectClass = "SpatRaster",
                  desc = paste("Binary raster representing with 1 represetnting harvested pixels and 0 non-harvested forest.",
                               "NA values represent non-forest")),
    createsOutput(objectName = "cumulativeHarvestMap", objectClass = "SpatRaster",
                  desc = "cumulative harvest in raster form"),
    createsOutput(objectName = "harvestSummary", objectClass = "data.table",
                  desc = "data.table with year and pixel index of harvested pixels"), 
    createsOutput(objectName = "thlb", objectClass = "SpatRaster", desc = "Harvestable pixels mask")
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
      sim <- scheduleEvent(sim, P(sim)$startTime, "simpleHarvest", "harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "simpleHarvest", "plot")
    },
    
    plot = {
      
      if (!is.null(sim$rstCurrentHarvest)) {
        # Annual harvest plot
        Plots(sim$rstCurrentHarvest,
              fn       = plot_raster,
              type     = P(sim)$.plots,
              filename = paste0("currentHarvest_year_", time(sim)),
              title    = paste0("Annual Harvest: year ", time(sim))
        )
        
        # Cumulative harvest plot
        Plots(sim$cumulativeHarvestMap,
              fn       = plot_raster,
              type     = P(sim)$.plots,
              filename = paste0("cumulativeHarvest_year_", time(sim)),
              title    = paste0("Cumulative Harvest: year ", time(sim))
        )
      }
      
      # Reschedule the next plot event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "simpleHarvest", "plot")
    },
    
    harvest = {
      
      # Generate the current harvest raster (SpatRaster)
      sim$rstCurrentHarvest <- harvestSpreadInputs(
        pixelGroupMap = sim$pixelGroupMap,
        cohortData = sim$cohortData,
        thlb = sim$thlb,
        spreadProb = P(sim)$spreadProb,
        maxCutSize = P(sim)$maxPatchSizetoHarvest,
        target = P(sim)$harvestTarget,
        minAgesToHarvest = P(sim)$minAgesToHarvest
      )
      # Update cumulative harvest map
      sim$cumulativeHarvestMap <- sim$rstCurrentHarvest + sim$cumulativeHarvestMap
      
      #Parvin added this part
      # Update timeSinceHarvest
      sim$timeSinceHarvest <- sim$timeSinceHarvest + 1  # increment all non-NA pixels
      harvested <- sim$rstCurrentHarvest > 0
      sim$timeSinceHarvest[harvested] <- 0
      # Keep unharvested areas as NA
      sim$timeSinceHarvest[is.na(sim$timeSinceHarvest)] <- NA
      
      # Convert raster values to a vector
      harvestVals <- terra::values(sim$rstCurrentHarvest, mat = FALSE)
      harvestVals <- as.numeric(harvestVals)  # Optional: only if needed
      harvestVals[is.na(harvestVals)] <- 0  # Replace NAs if necessary
      
      # Create a table of pixel indices harvested this year
      harvestIndex <- data.table(
        year = time(sim),
        pixelIndex = which(harvestVals == 1)  # Only keep harvested pixels
      )
      # Initialize harvestSummary if it doesn't exist
      if (is.null(sim$harvestSummary)) {
        sim$harvestSummary <- harvestIndex
      } else {
        sim$harvestSummary <- rbind(sim$harvestSummary, harvestIndex, fill = TRUE)
      }
      
      # Append to cumulative harvest summary
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
  sim$cumulativeHarvestMap <- sim$rasterToMatch
  names(sim$cumulativeHarvestMap) <- NULL
  
  sim$cumulativeHarvestMap[] <- 0
  sim$harvestSummary = data.table(year = integer(0), pixelIndex = integer(0))
  
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

  thlb <- terra::mask(thlb, pixelGroupMap )# Keeps values in thlb where pixelGroupMap is not NA
  
  #Make an ageMap
  # Make a biomass-weighted age map
  cohortData <- copy(cohortData)
  # Compute biomass-weighted average age per pixelGroup
  standAges <- cohortData[, .(BweightedAge = sum(B * age) / sum(B)), by = pixelGroup]
  
  # Extract raster values from pixelGroupMap and thlb
  pgVals <- terra::values(pixelGroupMap)[, 1]  # ensure it's a vector
  thlbVals <- terra::values(thlb)[, 1]
  
 # Combine values into a data.table and clean
  pixID <- data.table(
    pixelGroup = pgVals,
    pixelIndex = seq_len(ncell(pixelGroupMap)),
    thlb = thlbVals
  )
  
  # Remove NA rows and keep only thlb == 1 (productive forest)
  pixID <- na.omit(pixID)
  pixID <- pixID[thlb == 1]
  
  # Join biomass-weighted age info using pixelGroup
  landStats <- standAges[pixID, on = "pixelGroup"]
  
  # Keep only pixels with age above minimum threshold
  landStats <- landStats[BweightedAge >= minAgesToHarvest]
 
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
  
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # rasterToMatch mandatory (stop if missing)
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    stop("rasterToMatch must be supplied")
  }
  
  if (!suppliedElsewhere("cohortData", sim)) {
    sim$cohortData <- data.table::data.table(
      pixelGroup = 1L,
      age = 10L,
      species = "dummySpecies",
      biomass = 100
    )
  }
  
  # pixelGroupMap dummy from rasterToMatch 
  if (!suppliedElsewhere("pixelGroupMap", sim)) { 
    sim$pixelGroupMap <- terra::rast(sim$rasterToMatch) 
    vals <- terra::values(sim$pixelGroupMap) 
    vals[!is.na(vals)] <- 1L 
    terra::values(sim$pixelGroupMap) <- vals
    message("pixelGroupMap initialized from rasterToMatch.")}
  
  # thlb build from other layers if missing
  if (!suppliedElsewhere("thlb", sim)) {
    dem <- prepInputs(
      targetFile = "gtopo30Canada.tif",
      archive = NULL,
      url = NULL,
      to = sim$rasterToMatch,
      destinationPath = dPath,
      fun = "terra::rast",
      overwrite = FALSE, 
      userTags = c(cacheTags, "dem")
    )
    #Managed Forests of Canada #get map of forest management (2020) #values are 11 - long-term tenure, 12 - 
    #short-term tenure, 13 other, 20 Protected aras, #31 Federal reserve, 32 Indian Reserve, 33 Restricted, 
    #40 Treaty and Settlement, 50 Private forests #100 is water #no harvest on 100 (water), 32 (indian reserve), 
    #33 (Restricted), 20 (Protected), 13 (Other) #harvest on 11, 12, and 50
    
    # Load ManagedForest safely
    ManagedForest <- prepInputs(url = paste0("https://drive.google.com/file/d",
                                             "/1W2EiRtHj_81ZyKk5opqMkRqCA1tRMMvB/view?usp=share_link"),
                                fun = "terra::rast",
                                to = sim$rasterToMatch,
                                method = "near",
                                destinationPath = "GIS",
                                targetFile = "Canada_MFv2017.tif")
    
    # Raster to match
    rtm <- sim$rasterToMatch
    if (!inherits(rtm, "SpatRaster")) rtm <- terra::rast(rtm)
    rtm[!is.na(rtm)] <- 1
    
    # Initial thlb based on elevation
    thlb <- terra::mask(dem < 2000, rtm)
    
    # Clean ManagedForest first 
    thlb <- terra::mask(x = rtm, maskvalues = c(11, 12, 50), mask = ManagedForest, inverse = TRUE)
    
    sim$thlb <- thlb
  }
  
  # Initialize timeSinceHarvest if missing
  if (!suppliedElsewhere("timeSinceHarvest", sim)) {
    sim$timeSinceHarvest <- rast(sim$rasterToMatch)
    values(sim$timeSinceHarvest) <- NA   # NA = never harvested
  }
  
  return(sim)
}


