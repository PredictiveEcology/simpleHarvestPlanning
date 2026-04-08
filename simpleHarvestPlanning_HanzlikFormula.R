# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "simpleHarvestPlanning",
  description = paste("This is a very simplistic harvest module designed to interface with the LandR suite of modules",
                      "It will create a raster of harvested patches, but will not simulate actual harvest.", 
                      "the harvest target is calculated using Hanzlik fromula.",
                      "Should be paired with LandR_reforestation."),
  
  keywords = c("harvest", "LandR", "rstCurrentHarvest"),
  authors = c(
    person(c("Ian"), "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
    person("Parvin", "Kalantari", email = "parvin.kalantari@nrcan-rncan.gc.ca", role = c("aut","ctb"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9008", simpleHarvestPlanning = "0.0.1"),
  #spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "simpleHarvestPlanning.Rmd"),
  reqdPkgs = list("PredictiveEcology/LandR@development (>= 1.1.5.9099)", 'sf', "terra"),
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
    defineParameter("nBlocks", "numeric", 1L, min = 1L, max = 5L,
                    "Number of harvest blocks (spatial partitions of THLB)"),
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA,
                    desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 1, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 1 will likely result in harvest patches that are maximum size"))
    # defineParameter("verbose", "numeric", 0, 0, 1, 
    #                 desc = "if 1, print more detailed messaging about harvest")
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
    createsOutput(objectName = "harvestPerformance", objectClass = "list",
                  desc = "List with observed vs expected harvest summaries per year and per block"),
    createsOutput(objectName = "thlb", objectClass = "SpatRaster",
                  desc = "Harvestable pixels mask"))
  
))
#---------------------------------------------------------------------------------------------------
doEvent.simpleHarvesPlanning = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "simpleHarvestPlanning", "harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "simpleHarvestPlanning", "plot")
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
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "simpleHarvestPlanning", "plot")
    },
    
    #Harvest function
    
    # Harvest target using Hanzlik's formula
    harvest = {
      
      year <- as.integer(time(sim))  # Current simulation year
      
      # Add pixel info to cohortData
      cdLong <- LandR::addPixels2CohortData(sim$cohortData, sim$pixelGroupMap)
      
      # Attach blockId from raster
      cdLong[, blockId := terra::values(sim$blockId)[pixelIndex]]
      
      # --- Initialize Hanzlik target for each block
      target <- list()
      
      blocks <- sort(unique(na.omit(terra::values(sim$blockId))))
      
      for (bv in blocks) {
        
        Vm <- cdLong[blockId == bv & age >= P(sim)$minAgesToHarvest,
                     sum(B, na.rm = TRUE)]
        
        if (Vm <= 0) {
          target[[as.character(bv)]] <- 0
        } else {
          target[[as.character(bv)]] <- 1 / P(sim)$minAgesToHarvest
        }
      }
      
      # --- Run harvest
      harvestSpread <- harvestSpreadInputs(
        pixelGroupMap  = sim$pixelGroupMap,
        cohortData     = sim$cohortData,
        thlb           = sim$thlb,
        blockId        = sim$blockId,
        target        = target,      
        spreadProb     = P(sim)$spreadProb,
        maxCutSize     = P(sim)$maxPatchSizetoHarvest,
        minAgesToHarvest = P(sim)$minAgesToHarvest,
        year           = year
      )
      
      # Update simulation objects
      sim$rstCurrentHarvest <- harvestSpread$rstCurrentHarvest
      sim$speciesHarvestMaps <- harvestSpread$speciesHarvestMaps
      

      # Initialize once at start
      if (is.null(sim$harvestPerformance)) sim$harvestPerformance <- list()
      
      # Store this year's performance in the list
      sim$harvestPerformance[[as.character(year)]] <- harvestSpread$harvestPerformance
      
      # Accumulate harvestStats across years
      sim$harvestStats <- rbind(sim$harvestStats, harvestSpread$harvestStats, fill = TRUE)
      
      # --- Update cumulative harvest map
      if (is.null(sim$cumulativeHarvestMap)) {
        sim$cumulativeHarvestMap <- sim$rstCurrentHarvest
      } else {
        sim$cumulativeHarvestMap <- sim$cumulativeHarvestMap + (sim$rstCurrentHarvest > 0)
      }
      
      if (max(as.vector(sim$cumulativeHarvestMap),na.rm = TRUE) > 1) {
        browser()
      }
      
      # Update timeSinceHarvest: +1 for all non-NA pixels, reset harvested pixels to 0
      sim$timeSinceHarvest <- sim$timeSinceHarvest + 1
      
      # Identify harvested pixels
      harvested_vals <- as.vector(sim$rstCurrentHarvest)
      harvested_index <- which(harvested_vals > 0)
      sim$timeSinceHarvest[harvested_index] <- 0
      
      # --- Update harvestSummary
      #pixelIndex <- which(!is.na(terra::values(sim$pixelGroupMap)) & harvested_vals)
      pixelIndex <- which(!is.na(terra::values(sim$pixelGroupMap)) & harvested_vals > 0)
      
      pixelGroup <- terra::values(sim$pixelGroupMap)[pixelIndex]
      
      harvestIndex <- data.table(
        year       = year,
        pixelGroup = pixelGroup,
        pixelIndex = pixelIndex
      )[, .SD[1], by = .(year, pixelIndex)]
      
      sim$harvestSummary <- rbind(
        sim$harvestSummary,
        merge(
          harvestIndex,
          cdLong[, .(pixelGroup, pixelIndex, speciesCode, age, B)],
          by = c("pixelGroup", "pixelIndex"),
          all.x = TRUE
        ),
        fill = TRUE
      )
      
      # --- Save outputs
      folders <- c("blockId", "annualHarvest", "cumulativeHarvest", "speciesHarvest")
      for(f in folders) dir.create(file.path("outputs", f), showWarnings = FALSE, recursive = TRUE)
      
      terra::writeRaster(sim$blockId, file.path("outputs", "blockId", paste0("blockId_year_", year, ".tif")), overwrite = TRUE)
      terra::writeRaster(sim$rstCurrentHarvest, file.path("outputs", "annualHarvest", paste0("currentHarvest_year_", year, ".tif")), overwrite = TRUE)
      terra::writeRaster(sim$cumulativeHarvestMap, file.path("outputs", "cumulativeHarvest", paste0("cumulativeHarvest_year_", year, ".tif")), overwrite = TRUE)
      
      for(sp in names(sim$speciesHarvestMaps)) {
        terra::writeRaster(sim$speciesHarvestMaps[[sp]], file.path("outputs", "speciesHarvest", paste0("harvest_", sp, "_year_", year, ".tif")), overwrite = TRUE)
      }
      
      # --- Schedule next events
      sim <- scheduleEvent(sim, time(sim) + 1, "simpleHarvestPlanning", "harvest")
      sim <- scheduleEvent(sim, time(sim), "simpleHarvestPlanning", "plot")
      
    },
    
    warning(paste("Undefined event type: '",
                  current(sim)[1, "eventType", with = FALSE],
                  "' in module '",
                  current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  return(invisible(sim))
}

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
  
  # Initialize once at start
  if (is.null(sim$harvestPerformance)) sim$harvestPerformance <- list()
  
  return(invisible(sim))
}

#---------------------------------------------------------------------------------------------------
harvestSpreadInputs <- function(pixelGroupMap,
                                cohortData,
                                thlb,
                                blockId,
                                spreadProb,
                                maxCutSize,
                                minAgesToHarvest,
                                target,
                                year) {
  
  # Initialize outputs
  rstCurrentHarvest  <- terra::rast(pixelGroupMap)
  rstCurrentHarvest[] <- 0
  
  thlb <- terra::mask(thlb, pixelGroupMap)
  
  speciesHarvestMaps <- list()
  harvestStats <- data.table()

  # Biomass-weighted age per pixelGroup
  cohortData <- copy(cohortData)
  cohortData[, maxB :=  max(B), .(pixelGroup)]
  standAges <- cohortData[, BweightedAge := sum(B * age) / sum(B), by = .(pixelGroup)]
  standAges <- standAges[B == maxB,]
  
  #safety-catch in case maxB is tied between two species
  standAges[, N := .N, .(pixelGroup)]
  if (any(standAges$N > 1)) {
    standAges[, foo := runif(nrow(standAges))]
    standAges[, maxfoo := max(foo), .(pixelGroup)]
    standAges <- standAges[foo == maxfoo]
    standAges[, c("foo", "maxfoo", "maxB") := NULL]
  }
  
  # Prepare pixel information
  pgVals <- terra::values(pixelGroupMap)[,1]
  thlbVals <- terra::values(thlb)[,1]
  
  pixID <- data.table(pixelGroup = pgVals,
                      pixelIndex = seq_len(ncell(pixelGroupMap)),
                      thlb = thlbVals)
  pixID <- na.omit(pixID)
  pixID <- pixID[thlb == 1]

  # Compute biomass-weighted age per pixelGroup
  landStats <- standAges[pixID, on = "pixelGroup"]
  
  # --- Age filtering ---
  tooYoungPixels <- landStats[BweightedAge < minAgesToHarvest, pixelIndex]
  tooyoungPixelGroups <- as.vector(pixelGroupMap)[tooYoungPixels]
  
  landStats <- landStats[BweightedAge >= minAgesToHarvest]
  landStats[, blockId := terra::values(blockId)[pixelIndex]]
  landStats <- landStats[!is.na(pixelIndex)]
  
  landStats[pixelIndex %in% tooYoungPixels]

  # Harvestable areas raster
  harvestableAreas <- terra::rast(thlb)
  harvestableAreas[] <- 0
  harvestableAreas[landStats$pixelIndex] <- spreadProb
  harvestableAreas[is.na(harvestableAreas)] <- 0

  uniqueBlocks <- sort(unique(na.omit(terra::values(blockId))))
  uniqueBlocksChar <- as.character(uniqueBlocks)
  
  # Make sure target has names
  if (is.null(names(target))) names(target) <- as.character(seq_along(target))
  
  missingBlocks <- setdiff(uniqueBlocksChar, names(target))
  if (length(missingBlocks) > 0) stop("Missing harvest target: ", paste(missingBlocks, collapse = ", "))
  
  extratarget <- setdiff(names(target), uniqueBlocksChar)
  if (length(extratarget) > 0) warning("Extra target provided: ", paste(extratarget, collapse = ", "))

  for (b in uniqueBlocks) {
    pixelsInBlock <- landStats[blockId == b, pixelIndex]
    ht_block <- target[[as.character(b)]]
    if (length(pixelsInBlock) == 0 || is.null(ht_block)) next
    
    # species_in_block <- unique(cohortData[pixelIndex %in% pixelsInBlock, speciesCode])
    species_in_block <- unique(landStats[pixelIndex %in% pixelsInBlock, speciesCode])
    
    for (sp in species_in_block) {
      ht <- NA_real_
      if (is.list(ht_block)) {
        ht <- ht_block[[sp]]           # species-specific
        if (is.null(ht)) ht <- ht_block$default
        ht <- as.numeric(ht[1])        # assign numeric back to ht
      } else {
        ht <- as.numeric(ht_block)     # assign numeric back to ht
      }
      
      if (is.na(ht) || ht <= 0) next
      
      # Pixels for this species
      pix_sp <- unique(landStats[pixelIndex %in% pixelsInBlock & speciesCode %in% sp, pixelIndex])
      pix_sp <- pix_sp[!is.na(pix_sp)]
      if (length(pix_sp) == 0) next
      
      # Number of pixels to harvest
      availablePixels <- pix_sp
      
      availablePixels <- availablePixels[!availablePixels %in% tooYoungPixels]
      
      nAvailable <- length(availablePixels)
      
      # Target pixels
      # nPix <- round(ht * nAvailable)      
      nPix <- ceiling(ht * nAvailable)
      if (nPix == 0) next
      
      minCuts <- ceiling(nPix / maxCutSize)         # minimum number of patches
      maxHarvest <- minCuts * maxCutSize            # theoretical maximum harvest
      
      feasibleMax <- min(maxHarvest, nAvailable)    # cannot exceed available pixels
      
      # Seed pixels for spread
      initialCuts <- unique(sample(availablePixels, size = minCuts, replace = FALSE))
      
      # Remove too-young pixels
      initialCuts <- initialCuts[!initialCuts %in% tooYoungPixels]
      initialCuts <- initialCuts[!is.na(initialCuts)]
      if (length(initialCuts) == 0) next
  
      # Harvestable raster for species
      # Make too-young pixels unavailable in raster
      harvestableAreas[tooYoungPixels] <- 0
      harvestableAreasWithSp <- harvestableAreas
  
      unavailable <- which(!1:ncell(harvestableAreas) %in% pix_sp)
      harvestableAreasWithSp[unavailable] <- 0
      
      # Spread algorithm
      iteration <- spread2(
        landscape = harvestableAreasWithSp,
        start = initialCuts,
        asRaster = FALSE,
        spreadProb = harvestableAreasWithSp,
        maxSize = maxCutSize
      )
      
      if (nrow(iteration) == 0) next
      
      totalCut <- nrow(iteration)
      
      # Update rasters
      rstCurrentHarvest[iteration$pixels] <- 1
      harvestableAreas[iteration$pixels] <- 0
      
      illegalHarvest <- terra::values(rstCurrentHarvest) %in% tooYoungPixels
      
      if (any(iteration$pixels %in% tooYoungPixels)) {
        cat("WARNING: Some too-young pixels were harvested!\n")
        browser()
      }
      
      # Update species-specific harvest maps
      spHarvest <- terra::rast(pixelGroupMap)
      spHarvest[] <- 0
      spHarvest[iteration$pixels] <- 1
      speciesHarvestMaps[[sp]] <- if (is.null(speciesHarvestMaps[[sp]])) spHarvest else speciesHarvestMaps[[sp]] + spHarvest
      
      # ----------------------
      # Verbose live printing
      totalPixForSpecies <- sum(landStats[pixelIndex %in% pixelsInBlock, speciesCode] == sp)
      
      availablePixels <- pix_sp
      nAvailable <- length(availablePixels)
      
      # Demand (species-level, block-level)
      expectedHarvest_sp <- nPix
      
      # Spatial feasibility (diagnostic only)
      maxFeasiblePixels <- feasibleMax
     
      # Observed
      observedHarvest_sp <- totalCut
      
      # Discrepancy vs demand
      discrepancy_sp <- observedHarvest_sp - expectedHarvest_sp
      
      status_sp <- if (discrepancy_sp > 0) {
        "Over"
      } else if (discrepancy_sp < 0) {
        "Under"
      } else {
        "OK"
      }
      
      reason_sp <- if (status_sp == "Under") {
        if (expectedHarvest_sp > maxFeasiblePixels) {
          "Demand exceeds spatial feasibility"
        } else {
          "Spatial / stochastic limitation"
        }
      } else ""
      
      message(sprintf(
        "Year %d | Block %s | Species %s | Obs %d | Exp %d | MaxFeas %d | %s",
        year, b, sp, observedHarvest_sp, expectedHarvest_sp, maxFeasiblePixels, status_sp
      ))
      flush.console()
      
      
      # Update harvestStats table
      harvestStats <- rbind(
        harvestStats,
        data.table(
          year = year,
          blockId = b,
          speciesCode = sp,
          maxCutSize = maxCutSize,
          minCuts = minCuts,
          observedHarvest_sp = totalCut,
          expectedHarvest_sp = expectedHarvest_sp,
          nAvailable = nAvailable,
          targetUsed = ht,
          nPix = nPix,
          pixelIndex = list(iteration$pixels),
          pixelGroup = list(pgVals[iteration$pixels]),
          pix_sp = list(pix_sp),
          initialCuts = list(initialCuts)
        )
      )
    } # end species loop
  } # end block loop
  
  # Harvest performance diagnostics (PIXEL-BASED, LANDIS-CONSISTENT)
  # Ensure harvestStats is a data.table
  setDT(harvestStats)
  
  # Species-level comparison
  speciesComparison <- copy(harvestStats)
  
  speciesComparison[, discrepancy := observedHarvest_sp - expectedHarvest_sp]
  speciesComparison[, ratio_raw := fifelse(expectedHarvest_sp > 0,
                                           observedHarvest_sp / expectedHarvest_sp,
                                           NA_real_)]
  speciesComparison[, ratio := round(ratio_raw, 2)]
  speciesComparison[, status := fifelse(
    observedHarvest_sp == expectedHarvest_sp, "OK",
    fifelse(
      observedHarvest_sp < expectedHarvest_sp,
      "Under",
      "Over"
    )
  )]
  
  # add spatial feasibility (max feasible pixels) if needed
  # speciesComparison[, maxFeasible := ceiling(nPix / maxCutSize) * maxCutSize] # example if you want
  
  # Block-level summary
  blockComparison <- speciesComparison[, .(
    observedHarvest = sum(observedHarvest_sp),
    expectedHarvest = sum(expectedHarvest_sp)
  ), by = .(year, blockId)]
  
  blockComparison[, discrepancy := observedHarvest - expectedHarvest]
  blockComparison[, ratio := round(fifelse(expectedHarvest > 0,
                                           observedHarvest / expectedHarvest,
                                           NA_real_), 2)]
  blockComparison[, status := fifelse(
    observedHarvest == expectedHarvest, "OK",
    fifelse(observedHarvest < expectedHarvest, "Under", "Over")
  )]
  
  # Create final harvestPerformance list
  harvestPerformance <- list(
    blockYear = blockComparison,           # block/year summary
    species   = speciesComparison[, .(year, blockId, speciesCode, expectedHarvest_sp, observedHarvest_sp, targetUsed, ratio, status)],
    observed  = harvestStats[, .(year, blockId, speciesCode, observedHarvest_sp)],
    demand    = harvestStats[, .(year, blockId, speciesCode, expectedHarvest_sp, targetUsed)]
  )
  
  # Return all results
  return(list(
    rstCurrentHarvest  = rstCurrentHarvest,
    speciesHarvestMaps = speciesHarvestMaps,
    harvestStats       = harvestStats,
    harvestPerformance = harvestPerformance
  ))
}

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
  if (!suppliedElsewhere("blockId", sim)) {
    
    blockId <- sim$thlb
    blockId[] <- NA  # initialize
    
    validPixels <- which(!is.na(sim$thlb[]))  # pixels that can be harvested
    N <- P(sim)$nBlocks
    
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