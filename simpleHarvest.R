
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
    # Simulation/plotting controls
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "This is here for backwards compatibility. Please use `.plots`"),
    defineParameter(".plots", "character", "png", NA, NA,
                    "This describes the type of 'plotting' to do. See `?Plots` for possible types. To omit, set to NA"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, 
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?
                    This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    # Harvest controls
    defineParameter("startTime", "numeric", start(sim), NA, NA,
                    desc = "Simulation time at which to initiate harvesting"),
    defineParameter("harvestTarget", "numeric", 0.01, min = 0, max = 1,
                    desc= "proportion of harvestable area to harvest each timestep"),
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA, 
                    desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 0.24, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 0.24 will likely result in harvest patches that are maximum size")),
    defineParameter("blockTargets", "numeric", NA, NA, NA,
                    "Optional: vector of per-block harvest targets (named by blockId).
                   If NA/NULL, defaults to 1 (harvest all eligible pixels).")
  ),
  
  # Modified for terra: 
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", 
                 desc = "table with pixelGroup, age, species, and biomass of cohorts"),
    expectsInput(objectName = "pixelGroupMap", objectClass = "SpatRaster", 
                 desc = "Raster of pixelGroup locations"),   
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster", 
                 desc = "Template raster"),                         
    expectsInput(objectName = "studyArea", objectClass = "SpatVector", 
                 desc = "Study area polygon",                            
    expectsInput(objectName = "thlb", objectClass = "SpatRaster", 
                 desc = "Harvestable pixels mask"),                           
    expectsInput(objectName ="timeSinceHarvest", objectClass = "SpatRaster", 
                 desc = "map of time since last harvest; new harvests start at 0"))
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
    createsOutput(objectName = "thlb", objectClass = "SpatRaster", 
                  desc = "Harvestable pixels mask")
  )
))

doEvent.simpleHarvest = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
   
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "simpleHarvest", "harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "simpleHarvest", "plot")
    },
    
    plot = {
      
      if (!is.null(sim$rstCurrentHarvest)) {
        # Plot annual harvest
        Plots(sim$rstCurrentHarvest,
              fn       = plot_raster,
              type     = P(sim)$.plots,
              filename = paste0("currentHarvest_year_", time(sim)),
              title    = paste0("Annual Harvest: year ", time(sim))
        )
        
        # Plot cumulative harvest plot
        Plots(sim$cumulativeHarvestMap,
              fn       = plot_raster,
              type     = P(sim)$.plots,
              filename = paste0("cumulativeHarvest_year_", time(sim)),
              title    = paste0("Cumulative Harvest: year ", time(sim))
        )
      }
      
      # Reschedule the next plot event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, 
                           "simpleHarvest", "plot")
    },
    
    harvest = {
      
      # Generate the current harvest raster (SpatRaster)
      sim$rstCurrentHarvest <- harvestSpreadInputs(
        pixelGroupMap = sim$pixelGroupMap,
        cohortData = sim$cohortData,
        thlb = sim$thlb,
        target = P(sim)$harvestTarget,
        minAgesToHarvest = P(sim)$minAgesToHarvest,
        spreadProb = P(sim)$spreadProb,
        maxCutSize = P(sim)$maxPatchSizetoHarvest,
        blockId = sim$blockId,
        blockTargets = P(sim)$blockTargets
      ) 
      # Update cumulative harvest map
      sim$cumulativeHarvestMap <- sim$rstCurrentHarvest + sim$cumulativeHarvestMap
      
      # Update timeSinceHarvest: +1 everywhere harvestable, 0 where harvested this year
      # Increment all non-NA pixels by 1
      sim$timeSinceHarvest <- sim$timeSinceHarvest + 1
      
      # Extract raster values as a vector
      th_vals <- terra::values(sim$timeSinceHarvest, mat = FALSE)
      harvested_vals <- terra::values(sim$rstCurrentHarvest, mat = FALSE) == 1
      
      # Set harvested pixels to 0
      th_vals[harvested_vals] <- 0
      
      # Reassign back to raster
      terra::values(sim$timeSinceHarvest) <- th_vals

      # Create a table of pixel indices harvested this year
      harvestIndex <- data.table(
        year = time(sim),
        pixelIndex = which(harvested_vals)  # Only keep harvested pixels
      )
      
      sim$harvestSummary <- rbind(sim$harvestSummary, harvestIndex, fill = TRUE)
    
      # Append to cumulative harvest summary
      sim <- scheduleEvent(sim, time(sim) + 1,  "simpleHarvest", "harvest")
      
    },
    
    warning(paste("Undefined event type: '", 
                  current(sim)[1, "eventType", with = FALSE],
                  "' in module '", 
                  current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  return(invisible(sim))
}


# Initialization
Init <- function(sim) {
  
  # Initialize cumulative harvest raster
  sim$cumulativeHarvestMap <- sim$rasterToMatch
  sim$cumulativeHarvestMap[] <- 0
  
  # Initialize harvest summary table
  sim$harvestSummary = data.table(year = integer(0), pixelIndex = integer(0))
  
  # Ensure thlb is initialized
  if (is.null(sim$thlb)) {
    sim$thlb <- sim$rasterToMatch  # or your actual THLB raster
    sim$thlb[] <- 1                # 1 = harvestable, adjust as needed
  }
  
  # Ensure blockId is initialized
  if (is.null(sim$blockId)) {
    sim$blockId <- sim$thlb
    sim$blockId[] <- 1             # default all pixels = block 1
  }
  # Initialize timeSinceHarvest as a SpatRaster
  if (is.null(sim$timeSinceHarvest)) {
    sim$timeSinceHarvest <- sim$rasterToMatch
    sim$timeSinceHarvest[] <- 0   # 0 = never harvested
  }
  return(invisible(sim))
}

### template for plot events
# plotFun <- function(sim) {
#   
#   plot(sim$rstCurrentHarvest)
#   
#   return(invisible(sim))
# }

# Harvest spread function
harvestSpreadInputs <- function(pixelGroupMap,
                                cohortData,
                                thlb,
                                spreadProb,
                                maxCutSize,
                                minAgesToHarvest,
                                target,
                                blockId,
                                blockTargets) {
  
 
  #  Handle blockTargets
  uniqueBlocks <- sort(unique(na.omit(terra::values(blockId))))
  uniqueBlocksChar <- as.character(uniqueBlocks)
  
  if (is.null(blockTargets) || all(is.na(blockTargets))) {
    # Default: harvest all pixels in each block
    blockTargets <- setNames(rep(1, length(uniqueBlocksChar)), uniqueBlocksChar)
  } else {
    # Ensure blockTargets is numeric and named by blockId
    blockTargets <- as.numeric(blockTargets)
    names(blockTargets) <- uniqueBlocksChar
  }
  
  # initialize harvest rasters 
  rstCurrentHarvest <- terra::rast(pixelGroupMap)
  rstCurrentHarvest[] <- 0
  thlb <- terra::mask(thlb, pixelGroupMap)

  # Biomass-weighted age per pixelGroup
  cohortData <- copy(cohortData)
  standAges <- cohortData[, .(BweightedAge = sum(B * age) / sum(B)), by = pixelGroup]
  
  # Extract raster values from pixelGroupMap and thlb
  pgVals <- terra::values(pixelGroupMap)[, 1]  # ensure it's a vector
  thlbVals <- terra::values(thlb)[, 1]
  
  # Combine values into a data.table and clean
  pixID <- data.table(pixelGroup = pgVals,
                      pixelIndex = seq_len(ncell(pixelGroupMap)),
                      thlb = thlbVals
  )
  
  # Keep only productive forest pixels and remove NAs
  pixID <- pixID[!is.na(pixelGroup) & thlb == 1]
  # Join biomass-weighted age info using pixelGroup
  landStats <- standAges[pixID, on = "pixelGroup"]
  # Keep pixels above minimum age
  landStats <- landStats[BweightedAge >= minAgesToHarvest]
  
  # Raster for spread2 
  harvestableAreas <- terra::rast(thlb)
  harvestableAreas[] <- NA
  harvestableAreas[landStats$pixelIndex] <- spreadProb
  
  # Iterate over blocks to assign harvested pixels according to blockTargets
  # Block-based harvest allocation
  for (b in uniqueBlocks) {
    pixelsInBlock <- landStats[
      pixelGroup %in% pixID[pixelIndex %in% which(terra::values(blockId) == b)]$pixelGroup,
      pixelIndex]
    
    bt <- as.numeric(blockTargets[as.character(b)])
    if (length(pixelsInBlock) == 0 || is.na(bt) || bt <= 0) next
    
    nPix <- round(length(pixelsInBlock) * bt)
    if (nPix > 0) {
      harvested <- sample(pixelsInBlock, size = min(nPix, length(pixelsInBlock)), replace = FALSE)
      rstCurrentHarvest[harvested] <- 1
    }
  }
  
  # Targeted harvest spread
  harvestTarget <- round(nrow(landStats) * target)
  if (is.na(harvestTarget) || harvestTarget <= 0) return(rstCurrentHarvest)
  
  minCuts <- round(harvestTarget/maxCutSize)
  
  #calculate initial cuts by assuming every cut reaches the max
  initialCuts <- sample(landStats$pixelIndex, size = minCuts, replace = FALSE)
  
  iteration <- spread2(
    landscape = harvestableAreas,
    start = initialCuts,
    asRaster = FALSE,
    spreadProb = harvestableAreas,
    maxSize = maxCutSize
  )
  
  # Avoid NA assignment error inside spread2
  #successCells[!is.na(potentialNotAvailable) & !potentialNotAvailable] <- TRUE
  
  # Update harvest raster with spread
  rstCurrentHarvest[iteration$pixels] <- 1
  harvestableAreas[iteration$pixels] <- NA
  totalCut <- nrow(iteration)
  
 
  #0.97 creates a buffer so harvest is not guaranteed to exceed target rate
  # Keep adding until target is ~reached
  while (totalCut <= 0.97 * harvestTarget) {
    newCuts <- round((1 - totalCut / harvestTarget) * minCuts)
    newLocs <- landStats[!pixelIndex %in% iteration$pixels]$pixelIndex
    if (length(newLocs) == 0 || newCuts == 0) break
    
    newCutLocs <- sample(newLocs, size = min(newCuts, length(newLocs)), replace = FALSE)
    
    nextIteration <- spread2(
      landscape = harvestableAreas,
      start = newCutLocs,
      asRaster = FALSE,
      spreadProb = harvestableAreas,
      maxSize = maxCutSize
    )
    
    harvestableAreas[nextIteration$pixels] <- 0
    rstCurrentHarvest[nextIteration$pixels] <- 1
    
    totalCut <- totalCut + nrow(nextIteration)
   # minCuts <- c(minCuts + newCuts)
  }
  
  
  return(rstCurrentHarvest)
}


.inputObjects <- function(sim) {
  
  cacheTags <- c(currentModule(sim), "otherFunctions:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  #------------------------------------------------------------------------------
  
  # rasterToMatch mandatory (stop if missing)
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    stop("rasterToMatch must be supplied")
  }
  #------------------------------------------------------------------------------
  
  if (!suppliedElsewhere("cohortData", sim)) {
    sim$cohortData <- data.table::data.table(
      pixelGroup = 1L,
      age = 10L,
      species = "dummySpecies",
      biomass = 100
    )
  }
  #------------------------------------------------------------------------------ 
  
  # pixelGroupMap dummy from rasterToMatch 
  if (!suppliedElsewhere("pixelGroupMap", sim)) { 
    sim$pixelGroupMap <- terra::rast(sim$rasterToMatch) 
    vals <- terra::values(sim$pixelGroupMap) 
    vals[!is.na(vals)] <- 1L 
    terra::values(sim$pixelGroupMap) <- vals
    message("pixelGroupMap initialized from rasterToMatch.")
  }
  #------------------------------------------------------------------------------
  
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
    # ensure terra raster objects
    rtm <- sim$rasterToMatch
    if (!inherits(rtm, "SpatRaster")) rtm <- terra::rast(rtm)
    
    rtm[!is.na(rtm)] <- 1
    
    # Mask by elevation
    thlb <- terra::mask(dem < 2000 , rtm)
    
    # Keep only classes 11, 12, 50
    thlb <- terra::mask(x = rtm, mask = ManagedForest, maskvalues = c(11,12,50), inverse = TRUE)
    
    # Ensure thlb1 and thlb2 are 0/1
    # thlb1[is.na(thlb1[])] <- 0
    # thlb2[is.na(thlb2[])] <- 0
    
    # Combine
    # thlb <- thlb1 * thlb2  # multiply instead of &, keeps 0/1 and avoids NA
    
    sim$thlb <- thlb
    #sim$thlb[!is.na(rtm[]) & is.na(sim$thlb[])] <- 0 
  }
  #------------------------------------------------------------------------------
  
  # make sure sim$thlb exists first
  if (!suppliedElsewhere("blockId", sim)) {
    blockId <- sim$thlb
    blockId[] <- 1  # default: all = block 1
    
    # optional: split into 2 blocks for testing
    if (isTRUE(P(sim)$makeTwoBlocks)) {
      newVals <- (ncell(blockId)/2):ncell(blockId)
      newVals <- newVals[!is.na(sim$thlb[newVals])]
      blockId[newVals] <- 2
    }
    
    blockId[] <- as.numeric(blockId[])
    sim$blockId <- blockId
  }
  
  # consistency check: target vs. blockId
  
  blockVals <- unique(na.omit(values(sim$blockId)))
  if (!is.null(P(sim)$target)) {
    targetNames <- names(P(sim)$target)
    if (!all(targetNames %in% as.character(blockVals))) {
      stop("Some target names do not match blockId values: ",
           paste(setdiff(targetNames, blockVals), collapse = ", "))
    }
  }
  #------------------------------------------------------------------------------
  
  # Initialize timeSinceHarvest if missing
  if (!suppliedElsewhere("timeSinceHarvest", sim)) {
    sim$timeSinceHarvest <- rast(sim$rasterToMatch)
    values(sim$timeSinceHarvest) <- NA   # NA = never harvested
  }
  
  return(sim)
}


