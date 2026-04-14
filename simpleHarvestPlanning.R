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
    defineParameter("minAgesToHarvest", "numeric", 50, 1, NA,
                    desc =  "minimum ages of trees to harvest"),
    defineParameter("maxPatchSizetoHarvest", "numeric", 10, 1, NA,
                    desc = "maximum size for harvestable patches, in pixels"),
    defineParameter("spreadProb", "numeric", 1, 0.01, 1,
                    desc = paste("spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max.",
                                 "Exceeding 1 will likely result in harvest patches that are maximum size")),
    defineParameter("verbose", "numeric", 0, 0, 1,
                    desc = "if 1, print more detailed messaging about harvest")
  ),
  
  # inputObjects
  inputObjects = rbind(
    expectsInput(objectName = "planningArea", objectClass = "SpatRaster",
                 desc = "Raster of planning area ids"),
    expectsInput(objectName = "harvestTarget", objectClass = "list",
                 desc = "List containing the harvest targets for each planningArea. Can optionally be defined per species in a planningArea"),
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
                  desc = "List with observed vs expected harvest summaries per year and per planning area"),
    createsOutput(objectName = "thlb", objectClass = "SpatRaster",
                  desc = "Harvestable pixels mask"))
  
))
#---------------------------------------------------------------------------------------------------
doEvent.simpleHarvest = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "simpleHarvest", "harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "simpleHarvest", "plot")
    },
    
    plot = {
      if (!is.null(sim$rstCurrentHarvest)) {
        # Create a raster colored by planningArea for harvested pixels
        harvested_map <- sim$rstCurrentHarvest
        harvested_map[harvested_map > 0] <- sim$planningArea[harvested_map[] > 0]
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
    #---------------------------------------------------------------------------------------------------    
    harvest = {
      harvestSpread <- harvestSpreadInputs(
        pixelGroupMap = sim$pixelGroupMap,
        cohortData = sim$cohortData,
        thlb = sim$thlb,
        planningArea = sim$planningArea,
        spreadProb = P(sim)$spreadProb,
        maxCutSize = P(sim)$maxPatchSizetoHarvest,
        minAgesToHarvest = P(sim)$minAgesToHarvest,
        target = sim$harvestTarget,
        year = as.integer(time(sim)),
        verbose = P(sim)$verbose
      )
      
      # annual harvest map (binary)
      sim$rstCurrentHarvest <- harvestSpread$rstCurrentHarvest
      sim$speciesHarvestMaps <- harvestSpread$speciesHarvestMaps
      
      # update cumulative map if not exists
      if (is.null(sim$cumulativeHarvestMap)) {
        sim$cumulativeHarvestMap <- sim$rstCurrentHarvest
      } else {
        sim$cumulativeHarvestMap <- sim$cumulativeHarvestMap + (sim$rstCurrentHarvest > 0)
      }
      
      # time since harvest
      sim$timeSinceHarvest <- sim$timeSinceHarvest + 1
      sim$timeSinceHarvest[sim$rstCurrentHarvest] <- 0
      
      sim$speciesHarvestMaps <- harvestSpread$speciesHarvestMaps
      
      # Accumulate harvestStats across years
      sim$harvestStats <- rbind(sim$harvestStats, harvestSpread$harvestStats, fill = TRUE)
      
      # sim$harvestPerformance <- harvestSpread$harvestPerformance
      year <- as.integer(time(sim))  
      # Initialize once at start
      if (is.null(sim$harvestPerformance)) sim$harvestPerformance <- list()
      
      # Store this year's performance in the list
      sim$harvestPerformance[[as.character(year)]] <- harvestSpread$harvestPerformance
      
      # Identify harvested pixels
      harvested_vals <- as.vector(sim$rstCurrentHarvest)
      # harvested_index <- which(harvested_vals > 0)
      # sim$timeSinceHarvest[harvested_index] <- 0
      
      # Safeguard: only keep pixels with valid pixelGroup
      valid_pixels <- !is.na(terra::values(sim$pixelGroupMap))
      harvested_vals <- harvested_vals & valid_pixels
      
      # Now extract pixel indices and pixelGroup safely
      pixelIndex <- which(harvested_vals)
      pixelGroup <- terra::values(sim$pixelGroupMap)[harvested_vals]
      
      # Create a table of of harvested pixels
      harvestIndex <- data.table(
        year = as.integer(time(sim)),
        pixelGroup = pixelGroup,
        pixelIndex =  pixelIndex 
      )
      
      harvestIndex <- harvestIndex[, .SD[1], by = .(year, pixelIndex)]
      cdLong <- LandR::addPixels2CohortData(sim$cohortData, sim$pixelGroupMap)
      
      # Merge cohort data and append to harvestSummary
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
      # Add planningArea for each harvested pixel
      planningArea_vals <- terra::values(sim$planningArea)
      sim$harvestSummary[, planningArea := planningArea_vals[pixelIndex]]
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
      planningArea = integer(), # planningArea ID
      speciesCode = character(), # species code
      maxCutSize = numeric(), # max patch size
      minCuts = numeric(), # number of seeds
      observedHarvest_sp = numeric(), # number of harvested pixels
      expectedHarvest_sp = numeric(), # number of expected harvested pixels
      nPix = numeric(), # target number of pixels to harvest
      nAvailable = numeric(), # length of available pixels for harvesting
      targetUsed = numeric(), # 
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

harvestSpreadInputs <- function(pixelGroupMap,
                                cohortData,
                                thlb,
                                planningArea,
                                spreadProb,
                                maxCutSize,
                                minAgesToHarvest,
                                target,
                                year, 
                                verbose) {
  
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
  
  # Age filtering 
  tooYoungPixels <- landStats[BweightedAge < minAgesToHarvest, pixelIndex]
  tooyoungPixelGroups <- as.vector(pixelGroupMap)[tooYoungPixels]
  
  # Keep only eligible pixels
  landStats <- landStats[BweightedAge >= minAgesToHarvest]
  landStats[, planningArea := terra::values(planningArea)[pixelIndex]]
  landStats <- landStats[!is.na(pixelIndex)]
  
  landStats[pixelIndex %in% tooYoungPixels]
  
  # Harvestable areas raster
  harvestableAreas <- terra::rast(thlb)
  harvestableAreas[] <- 0
  harvestableAreas[landStats$pixelIndex] <- spreadProb
  harvestableAreas[is.na(harvestableAreas)] <- 0
  
  # Harvest loop: planningAreas -> species
  uniquePlanningAreas <- sort(unique(na.omit(terra::values(planningArea))))
  uniquePlanningAreaChar <- as.character(uniquePlanningAreas)
  
  # Make sure target has names
  if (is.null(names(target))) names(target) <- as.character(seq_along(target))
  
  missingPlanningAreas <- setdiff(uniquePlanningAreaChar, names(target))
  if (length(missingPlanningAreas) > 0) stop("Missing harvest targets: ", paste(missingPlanningAreas, collapse = ", "))
  
  extraTargets <- setdiff(names(target), uniquePlanningAreaChar)
  if (length(extraTargets) > 0) warning("Extra targets provided: ", paste(extraTargets, collapse = ", "))
  
  for (b in uniquePlanningAreas) {
    
    pixelsInPlanningArea <- landStats[planningArea == b, pixelIndex]
    ht_planningArea <- target[[as.character(b)]]
    if (length(pixelsInPlanningArea) == 0 || is.null(ht_planningArea)) next
    
    species_in_planningArea <- unique(landStats[pixelIndex %in% pixelsInPlanningArea, speciesCode])
    
    for (sp in species_in_planningArea) {
      ht <- NA_real_
      if (is.list(ht_planningArea)) {
        ht <- ht_planningArea[[sp]]           # species-specific
        if (is.null(ht)) ht <- ht_planningArea$default
        ht <- as.numeric(ht[1])        # assign numeric back to ht
      } else {
        ht <- as.numeric(ht_planningArea)     # assign numeric back to ht
      }
      
      if (is.na(ht) || ht <= 0) next
      
      # Pixels for this species
      pix_sp <- unique(landStats[pixelIndex %in% pixelsInPlanningArea & speciesCode %in% sp, pixelIndex])
      pix_sp <- pix_sp[!is.na(pix_sp)]
      if (length(pix_sp) == 0) next
      
      # Number of pixels to harvest
      # availablePixels <- pix_sp
      # Remove already harvested pixels from consideration
      availablePixels <- pix_sp
      
      availablePixels <- availablePixels[!availablePixels %in% tooYoungPixels]
      
      nAvailable <- length(availablePixels)
      if (length(availablePixels) == 0) next
      
      
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
      
      # Make too-young pixels unavailable in raster
      harvestableAreas[tooYoungPixels] <- 0
      harvestableAreasWithSp <- harvestableAreas
      
      #unavailable <- setdiff(1:ncell(harvestableAreas), pix_sp)
      unavailable <- which(!1:ncell(harvestableAreas) %in% pix_sp)
      harvestableAreasWithSp[unavailable] <- 0
      
      # Now run the spread algorithm safely
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
        message("WARNING: Some too-young pixels were harvested!\n")
      }
      
      # Update species-specific harvest maps
      spHarvest <- terra::rast(pixelGroupMap)
      spHarvest[] <- 0
      spHarvest[iteration$pixels] <- 1
      speciesHarvestMaps[[sp]] <- if (is.null(speciesHarvestMaps[[sp]])) spHarvest else speciesHarvestMaps[[sp]] + spHarvest
      
      # Verbose live printing
      totalPixForSpecies <- sum(landStats[pixelIndex %in% pixelsInPlanningArea, speciesCode] == sp)
      availablePixels <- pix_sp
      nAvailable <- length(availablePixels)
      
      # Demand (species-level, planning area-level)
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
      if (verbose) {
        message(sprintf(
          "Year %d | planningArea %s | Species %s | Obs %d | Exp %d | MaxFeas %d | %s",
          year, b, sp, observedHarvest_sp, expectedHarvest_sp, maxFeasiblePixels, status_sp
        ))
        flush.console()
      }
      
      # Update harvestStats table
      harvestStats <- rbind(
        harvestStats,
        data.table(
          year = year,
          planningArea = b,
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
  
  # planningArea-level summary
  planningAreaComparison <- speciesComparison[, .(
    observedHarvest = sum(observedHarvest_sp),
    expectedHarvest = sum(expectedHarvest_sp)
  ), by = .(year, planningArea)]
  
  planningAreaComparison[, discrepancy := observedHarvest - expectedHarvest]
  planningAreaComparison[, ratio := round(fifelse(expectedHarvest > 0,
                                                  observedHarvest / expectedHarvest,
                                                  NA_real_), 2)]
  planningAreaComparison[, status := fifelse(
    observedHarvest == expectedHarvest, "OK",
    fifelse(observedHarvest < expectedHarvest, "Under", "Over")
  )]
  
  # Create final harvestPerformance list
  harvestPerformance <- list(
    planningAreaYear = planningAreaComparison,           # planningArea/year summary
    species   = speciesComparison[, .(year, planningArea, speciesCode, expectedHarvest_sp, observedHarvest_sp, targetUsed, ratio, status)],
    observed  = harvestStats[, .(year, planningArea, speciesCode, observedHarvest_sp)],
    demand    = harvestStats[, .(year, planningArea, speciesCode, expectedHarvest_sp, targetUsed)]
  )
  
  # Return all results
  return(list(
    rstCurrentHarvest  = rstCurrentHarvest,
    speciesHarvestMaps = speciesHarvestMaps,
    harvestStats       = harvestStats,
    harvestPerformance = harvestPerformance
  ))
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
    message("pixelGroupMap initialized from rasterToMatch.")
  }
  
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
  
  if (!suppliedElsewhere("harvestTarget", sim)) {
    sim$harvestTarget <- list("1" = 0.01)
  }
  
  if (!suppliedElsewhere("planningArea", sim)) {
    
    planningArea <- sim$thlb
    planningArea[] <- NA  # initialize
    
    validPixels <- which(!is.na(sim$thlb[]))  # pixels that can be harvested
    N <- length(sim$harvestTarget)
    
    if (N == 1) {
      planningArea[validPixels] <- 1
    } else {
      pixelsPerplanningArea <- floor(length(validPixels) / N)
      
      for (i in 1:N) {
        startIdx <- (i - 1) * pixelsPerplanningArea + 1
        endIdx <- if (i < N) i * pixelsPerplanningArea else length(validPixels)
        selected <- validPixels[startIdx:endIdx]
        planningArea[selected] <- i
      }
    }
    
    planningArea[] <- as.numeric(planningArea[])
    sim$planningArea <- planningArea
  }
  
  #check if number of planningAreas matches those in harvestTargets
  if (!global(sim$planningArea, "max", na.rm = TRUE)[[1]] == length(sim$harvestTarget)) {
    stop("There is a mismatch between sim$planningArea and the areas defined in sim$harvestTargets")
  }
  
  # Initialize timeSinceHarvest if missing
  if (!suppliedElsewhere("timeSinceHarvest", sim)) {
    sim$timeSinceHarvest <- rast(sim$rasterToMatch)
    values(sim$timeSinceHarvest) <- NA   # NA = never harvested
  }
  
  return(sim)
}