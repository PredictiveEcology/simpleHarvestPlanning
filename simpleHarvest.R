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
    person("Parvin", "Kalantari", email = "parvin.kalantari@nrcan-rncan.gc.ca", role = c("aut","ctb"))
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
    # defineParameter("harvestTarget", "numeric", NA, min = 0, max = 1,
    #                 desc= "proportion of harvestable area to harvest each timestep"),
    defineParameter("harvestTarget", "list", NA,
                    desc= "Harvest targets per block (optionally per species within block)"),
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA,
                    desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 0.24, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 0.24 will likely result in harvest patches that are maximum size"))
  ),
  
  # inputObjects
  inputObjects = rbind(
    expectsInput(objectName = "blockId", objectClass = "SpatRaster",
                 desc = "Raster of block ids"),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "table with pixelGroup, age, species, and biomass of cohorts"),
    expectsInput(objectName = "cumulativeHarvestMap", objectClass = "SpatRaster",
                 desc = "cumulative harvest in raster form"),
    expectsInput(objectName = "pixelGroupMap", objectClass = "SpatRaster",
                 desc = "Raster of pixelGroup locations"),
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster",
                 desc = "Template raster"),
    expectsInput(objectName = "studyArea", objectClass = "SpatVector",
                 desc = "Study area polygon"),
    expectsInput(objectName = "thlb", objectClass = "SpatRaster",
                 desc = "Harvestable pixels mask"),
    expectsInput(objectName ="timeSinceHarvest", objectClass = "SpatRaster",
                 desc = "map of time since last harvest; new harvests start at 0")
  ),
  
  # objectClass
  outputObjects = rbind(
    createsOutput(objectName = "rstCurrentHarvest", objectClass = "SpatRaster",
                  desc = paste("Binary raster representing with 1 representing harvested pixels and 0 non-harvested forest.",
                               "NA values represent non-forest")),
    createsOutput(objectName = "cumulativeHarvestMap", objectClass = "SpatRaster",
                  desc = "cumulative harvest in raster form"),
    createsOutput(objectName = "harvestSummary", objectClass = "data.table",
                  desc = "data.table with year and pixel index of harvested pixels"),
    createsOutput(objectName = "harvestStats", objectClass = "data.table",
                  desc = "data.table with storage for minCuts, totalCut, and target"),
    createsOutput(objectName = "speciesHarvestMaps", objectClass = "list",
                  desc = "List of binary SpatRasters representing harvested pixels per species.
                          Each raster has 1 for harvested pixels and 0 for non-harvested pixels."),
    createsOutput(objectName = "thlb", objectClass = "SpatRaster",
                  desc = "Harvestable pixels mask"))
  
))
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
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
        
        # Create a raster colored by blockId for harvested pixels
        harvested_map <- sim$rstCurrentHarvest
        harvested_map[harvested_map > 0] <- sim$blockId[harvested_map[] > 0]
        harvested_map[harvested_map == 0] <- NA  # hide 0 values
        
        # Plot annual harvest
        Plots(harvested_map,
              fn       = plot_raster,
              type     = P(sim)$.plots,
              filename = paste0("currentHarvest_year_", time(sim)),
              title    = paste0("Annual Harvest: year ", time(sim))
        )
        
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
      
      # Run harvest function
      harvestSpread <- harvestSpreadInputs(
        pixelGroupMap = sim$pixelGroupMap,
        cohortData = sim$cohortData,
        thlb = sim$thlb,
        blockId = sim$blockId,
        spreadProb = P(sim)$spreadProb,
        maxCutSize = P(sim)$maxPatchSizetoHarvest,
        target = P(sim)$harvestTarget,
        minAgesToHarvest = P(sim)$minAgesToHarvest
      )
      
      browser()
      # Update simulation objects
      sim$rstCurrentHarvest <- harvestSpread$rstCurrentHarvest
      sim$speciesHarvestMaps  <- harvestSpread$speciesHarvestMaps
      
      sim$harvestStats <- rbind(sim$harvestStats, harvestSpread$harvestStats, fill = TRUE)
      
      # Update cumulative harvest map 
      
      # Initialize cumulative map if not exists
      if (is.null(sim$cumulativeHarvestMap)) {
        sim$cumulativeHarvestMap <- sim$rstCurrentHarvest
      } else {
        sim$cumulativeHarvestMap <- sim$cumulativeHarvestMap + (sim$rstCurrentHarvest > 0)
      }
      
      # Update timeSinceHarvest: +1 for all non-NA pixels, reset harvested pixels to 0
      sim$timeSinceHarvest <- sim$timeSinceHarvest + 1
      
      # Identify harvested pixels
      harvested_vals <- terra::values(sim$rstCurrentHarvest) > 0
      sim$timeSinceHarvest[harvested_vals] <- 0
      
      # Safeguard: only keep pixels with valid pixelGroup
      valid_pixels <- !is.na(terra::values(sim$pixelGroupMap))
      harvested_vals <- harvested_vals & valid_pixels
      
      # Now extract pixel indices and pixelGroup safely
      pixelIndex <- which(harvested_vals)
      pixelGroup <- terra::values(sim$pixelGroupMap)[harvested_vals]
      
      # Create a table of of harvested pixels
      harvestIndex <- data.table(
        year = as.integer(time(sim)),          # numeric year
        pixelGroup = pixelGroup,
        pixelIndex =  pixelIndex 
      )
      
      # Merge cohort data and append to harvestSummary
      sim$harvestSummary <- rbind(
        sim$harvestSummary,
        merge(
          harvestIndex,
          sim$cohortData[, .(pixelGroup, speciesCode, age, B)],
          by = "pixelGroup",
          all.x = TRUE
        ),
        fill = TRUE
      )
      
      # Add blockId for each harvested pixel
      block_vals <- terra::values(sim$blockId)
      sim$harvestSummary[, blockId := block_vals[pixelIndex]]
      
      # year <- as.integer(time(sim))
      # 
      # # Define folders
      # folders <- c("blockId", "annualHarvest", "cumulativeHarvest", "speciesHarvest")
      # for(f in folders){
      #   dir.create(file.path("outputs", f), showWarnings = FALSE, recursive = TRUE)
      # }
      # 
      # # Save blockId raster
      # terra::writeRaster(
      #   sim$blockId,
      #   file.path("outputs", "blockId", paste0("blockId_year_", year, ".tif")),
      #   overwrite = TRUE
      # )
      # 
      # # Save current harvest raster
      # terra::writeRaster(
      #   sim$rstCurrentHarvest,
      #   file.path("outputs", "annualHarvest", paste0("currentHarvest_year_", year, ".tif")),
      #   overwrite = TRUE
      # )
      # 
      # # Save cumulative harvest raster
      # terra::writeRaster(
      #   sim$cumulativeHarvestMap,
      #   file.path("outputs", "cumulativeHarvest", paste0("cumulativeHarvest_year_", year, ".tif")),
      #   overwrite = TRUE
      # )
      # 
      # # Save species-specific harvest maps
      # for(sp in names(sim$speciesHarvestMaps)) {
      #   terra::writeRaster(
      #     sim$speciesHarvestMaps[[sp]],
      #     file.path("outputs", "speciesHarvest", paste0("harvest_", sp, "_year_", year, ".tif")),
      #     overwrite = TRUE
      #   )
      # }
      
      # Schedule next harvest event
      sim <- scheduleEvent(sim, time(sim) + 1, "simpleHarvest", "harvest")
      # Schedule plot for current harvest
      sim <- scheduleEvent(sim, time(sim), "simpleHarvest", "plot")
      
    },
    
    warning(paste("Undefined event type: '",
                  current(sim)[1, "eventType", with = FALSE],
                  "' in module '",
                  current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  return(invisible(sim))
}
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# Initialization
Init <- function(sim) {
  
  # Initialize current harvest raster
  if (is.null(sim$rstCurrentHarvest)) {
    sim$rstCurrentHarvest <- sim$rasterToMatch
    sim$rstCurrentHarvest[] <- 0   # 0 = not harvested yet
  }
  
  # Initialize cumulative harvest raster
  if (is.null(sim$cumulativeHarvestMap)) {
    sim$cumulativeHarvestMap <- sim$rasterToMatch
    sim$cumulativeHarvestMap[] <- 0
  }
  
  # Initialize harvest summary table
  if (is.null(sim$harvestSummary)) {
    sim$harvestSummary <- data.table(
      year = integer(0),
      pixelIndex = integer(0),
      pixelGroup = integer(0),
      speciesCode = factor(),
      age = integer(0),
      B = integer(0)
    )
  }
  
  # Initialize storage for detailed harvest statistics
  if (is.null(sim$harvestStats)) {
    sim$harvestStats <- data.table(
      year = integer(), # simulation year
      blockId = integer(), # block ID
      speciesCode = character(), # species code
      maxCutSize = numeric(), # max patch size
      minCuts = numeric(), # number of seeds
      totalCut = numeric(), # number of harvested pixels
      nPix = numeric(), # target number of pixels to harvest
      pixelIndex = list(), # harvested pixel indices
      pixelGroup = list(), # pixelGroup values of harvested pixels
      pix_sp = list(), # all candidate pixels before selection
      initialCuts = list() # seed pixels used in spread
    )
  }
  
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
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# Harvest spread function
harvestSpreadInputs <- function(pixelGroupMap,
                                cohortData,
                                thlb,
                                blockId,
                                spreadProb,
                                maxCutSize,
                                minAgesToHarvest,
                                target) {
  
  
  speciesHarvestMaps <- list()   # initialize list
  
  harvestStats <- data.table()
  
  # Identify unique blocks
  uniqueBlocks <- sort(unique(na.omit(terra::values(blockId))))
  uniqueBlocksChar <- as.character(uniqueBlocks)
  
  # Make sure target has names
  if (is.null(names(target))) {
    names(target) <- as.character(seq_along(target))
  }
  
  # Check for missing targets
  missingBlocks <- setdiff(uniqueBlocksChar, names(target))
  if (length(missingBlocks) > 0) {
    stop(paste("Missing harvest target(s) for blockId(s):",
               paste(missingBlocks, collapse = ", ")))
  }
  
  # Check for extra targets
  extraTargets <- setdiff(names(target), uniqueBlocksChar)
  if (length(extraTargets) > 0) {
    warning(paste("Extra harvest target(s) provided that are not present in blockId:",
                  paste(extraTargets, collapse = ", ")))
  }
  
  # Prepare harvest raster layer
  rstCurrentHarvest <- terra::rast(pixelGroupMap)
  rstCurrentHarvest[!is.na(pixelGroupMap[])] <- 0
  thlb <- terra::mask(thlb, pixelGroupMap)
  
  # Biomass-weighted age per pixelGroup
  cohortData <- copy(cohortData)
  standAges <- cohortData[, .(BweightedAge = sum(B * age) / sum(B)), by = pixelGroup]
  
  # Extract raster values from pixelGroupMap and thlb
  pgVals <- terra::values(pixelGroupMap)[, 1]  # ensure vector
  thlbVals <- terra::values(thlb)[, 1]
  
  pixID <- data.table(pixelGroup = pgVals,
                      pixelIndex = seq_len(ncell(pixelGroupMap)),
                      thlb = thlbVals)
  pixID <- na.omit(pixID)
  pixID <- pixID[thlb == 1]
  
  landStats <- standAges[pixID, on = "pixelGroup"]
  landStats <- landStats[BweightedAge >= minAgesToHarvest]
  # Add blockId to landStats for correct filtering
  landStats[, blockId := terra::values(blockId)[pixelIndex]]
  # Remove any rows with missing pixelIndex
  landStats <- landStats[!is.na(pixelIndex)]
  
  harvestableAreas <- terra::rast(thlb)
  #harvestableAreas[] <- NA
  harvestableAreas[] <- 0 
  harvestableAreas[landStats$pixelIndex] <- spreadProb
  # Safety check before spread2
  harvestableAreas[is.na(harvestableAreas)] <- 0
  
  # Add pixelIndex to cohortData
  cohortData <- merge(cohortData, pixID[, .(pixelGroup, pixelIndex)], 
                      by = "pixelGroup", all.x = TRUE)
  # NA in pixID are pixels that cannot be harvested
  cohortData <- cohortData[!is.na(pixelIndex)]
  
  # Calculate leading species per pixel 
  cohortData_leading <- cohortData[, .SD[which.max(B)], by = pixelIndex]
  # Result: one row per pixelIndex, with the speciesCode that has the highest B
  
  # Loop over blocks
  for (b in uniqueBlocks) {
    
    pixelsInBlock <- landStats[blockId == b, pixelIndex]
    ht_block <- target[[as.character(b)]]
    if (length(pixelsInBlock) == 0 || is.null(ht_block)) next
    
    cat("Processing block:", b, 
        "| pixels:", length(pixelsInBlock), 
        "| target:", paste(capture.output(str(ht_block)), collapse = " "), "\n")
    
    
    species_in_block <- unique(cohortData[pixelIndex %in% pixelsInBlock, speciesCode])
    
    for (sp in species_in_block) {
      if (is.list(ht_block)) {
        # species-specific target, fallback to default
        val <- ht_block[[sp]]
        if (is.null(val)) val <- ht_block$default
        ht <- as.numeric(val[1])   # take the first element only
      } else {
        ht <- as.numeric(ht_block) # block-level target
      }
      if (is.na(ht) || ht <= 0) next
      
      pix_sp <- unique(cohortData[pixelIndex %in% pixelsInBlock & speciesCode == sp, pixelIndex])
      if (length(pix_sp) == 0) next
      
      nPix <- round(length(pix_sp) * ht)   # Number of pixels to harvest
      if (nPix == 0) next
      
      minCuts <- round(nPix / maxCutSize)
      totalCut <- NA  # initialize
      
      initialCuts <- unique(sample(pix_sp, size = minCuts, replace = FALSE))
      
      initialCuts <- initialCuts[!is.na(initialCuts)]
      if (length(initialCuts) == 0) next
      
      iteration <- spread2(
        landscape = harvestableAreas,
        start = initialCuts,
        asRaster = FALSE,
        spreadProb = harvestableAreas,
        maxSize = maxCutSize
      )
      
      # Update current harvest raster
      rstCurrentHarvest[iteration$pixels] <- 1
      #harvestableAreas[iteration$pixels] <- NA
      harvestableAreas[iteration$pixels] <- 0
      
      totalCut <- nrow(iteration)
      
      # Update species-specific harvest map
      spHarvest <- terra::rast(pixelGroupMap)
      spHarvest[!is.na(pixelGroupMap[])] <- 0
      spHarvest[iteration$pixels] <- 1
      speciesHarvestMaps[[sp]] <- if (is.null(speciesHarvestMaps[[sp]])) spHarvest else speciesHarvestMaps[[sp]] + spHarvest

      # Update harvestStats
      harvestStats <- rbind(
        harvestStats,
        data.table(
          year = as.integer(time(sim)),
          blockId = b,
          speciesCode = sp,
          maxCutSize = maxCutSize,
          minCuts = minCuts,
          totalCut = totalCut,
          nPix = nPix,
          pixelIndex = iteration$pixels,
          pixelGroup = pgVals[iteration$pixels],
          pix_sp = list(pix_sp),
          initialCuts = list(initialCuts)
        )
      )
      
      # =============================
      # Print summary per species per block
      # =============================
      cat(sprintf("  Species '%s' harvested %d pixels in block %s (target %.2f)\n",
                  sp, totalCut, b, ht))
      
    } # end species loop
    
  } # end block loop
  
  return(list(
    rstCurrentHarvest = rstCurrentHarvest,
    speciesHarvestMaps = speciesHarvestMaps,
    harvestStats = harvestStats
    
  ))
}
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
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
      url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/view?usp",
      targetFile = "gtopo30Canada.tif",
      destinationPath = dPath,
      fun = "terra::rast",
      overwrite = FALSE,
      to = sim$rasterToMatch,
      userTags = c(cacheTags, "dem")
    )
    
    # Managed Forests of Canada #get map of forest management (2020) #values are 11 - long-term tenure, 
    # 12 - short-term tenure, 13 other, 20 Protected aras, #31 Federal reserve, 32 Indian Reserve, 33 Restricted,
    # 40 -Treaty and Settlement, 50 Private forests #100 is water #no harvest on 100 (water), 32 (indian reserve),
    #harvest on 11, 12, and 50
    
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
    
    sim$thlb <- thlb
  }
  
  #------------------------------------------------------------------------------
  
  if (!is.null(P(sim)$harvestTarget)) {
    #sim$harvestTarget <- P(sim)$harvestTarget
    sim$target <- P(sim)$harvestTarget
  }
  
  #------------------------------------------------------------------------------
  if (!suppliedElsewhere("blockId", sim)) {
    
    blockId <- sim$thlb
    blockId[] <- NA  # initialize
    
    validPixels <- which(!is.na(sim$thlb[]))  # pixels that can be harvested
    N <- length(P(sim)$harvestTarget)
    
    if (N == 1) {
      blockId[validPixels] <- 1
    } else {
      pixelsPerBlock <- floor(length(validPixels) / N)
      
      for (i in 1:N) {
        startIdx <- (i - 1) * pixelsPerBlock + 1
        endIdx <- if (i < N) i * pixelsPerBlock else length(validPixels)
        selected <- validPixels[startIdx:endIdx]
        blockId[selected] <- i
      }
    }
    
    blockId[] <- as.numeric(blockId[])
    sim$blockId <- blockId
  }
  
  #------------------------------------------------------------------------------
  
  # Initialize timeSinceHarvest if missing
  if (!suppliedElsewhere("timeSinceHarvest", sim)) {
    sim$timeSinceHarvest <- rast(sim$rasterToMatch)
    values(sim$timeSinceHarvest) <- NA   # NA = never harvested
  }
  return(sim)
}