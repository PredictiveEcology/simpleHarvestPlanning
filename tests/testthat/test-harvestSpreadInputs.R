## harvestSpreadInputs(): which pixels may be cut, and how many.

test_that("only old-enough pixels on the thlb are cut", {
  L <- toyLandscape()
  L$thlb[1:2] <- NA  # two old pixels off the land base
  set.seed(1)
  out <- harvestSpreadInputs(
    pixelGroupMap = L$pixelGroupMap, cohortData = L$cohortData, thlb = L$thlb,
    planningArea = L$planningArea, spreadProb = 1, maxCutSize = 2, minAgesToHarvest = 50,
    target = list("1" = 0.5), year = 2020L, verbose = 0)
  cut <- which(terra::values(out$rstCurrentHarvest)[, 1] == 1)
  expect_gt(length(cut), 0)
  ## group 2 (pixels 9-16) has biomass-weighted age < 50; pixels 1-2 are off the thlb
  expect_true(all(cut %in% 3:8))
})

test_that("speciesHarvestMaps name the dominant species of each cut pixel", {
  L <- toyLandscape()
  set.seed(1)
  out <- harvestSpreadInputs(
    pixelGroupMap = L$pixelGroupMap, cohortData = L$cohortData, thlb = L$thlb,
    planningArea = L$planningArea, spreadProb = 1, maxCutSize = 2, minAgesToHarvest = 50,
    target = list("1" = 0.5), year = 2020L, verbose = 0)
  expect_named(out$speciesHarvestMaps, "Pice_mar")  # largest B in group 1
})

test_that("a target of 0 cuts nothing", {
  L <- toyLandscape()
  out <- harvestSpreadInputs(
    pixelGroupMap = L$pixelGroupMap, cohortData = L$cohortData, thlb = L$thlb,
    planningArea = L$planningArea, spreadProb = 1, maxCutSize = 2, minAgesToHarvest = 50,
    target = list("1" = 0), year = 2020L, verbose = 0)
  expect_identical(sum(terra::values(out$rstCurrentHarvest)[, 1], na.rm = TRUE), 0)
  expect_identical(nrow(out$harvestStats), 0L)
  expect_length(out$speciesHarvestMaps, 0)
})

test_that("cutCohorts() of an empty speciesHarvestMaps is empty", {
  L <- toyLandscape()
  expect_identical(nrow(timberSupply::cutCohorts(L$cohortData, L$pixelGroupMap, list(), 50)), 0L)
})

test_that("a target for a planningArea that does not exist is an error", {
  L <- toyLandscape()
  L$planningArea[9:16] <- 2
  expect_error(
    harvestSpreadInputs(
      pixelGroupMap = L$pixelGroupMap, cohortData = L$cohortData, thlb = L$thlb,
      planningArea = L$planningArea, spreadProb = 1, maxCutSize = 2, minAgesToHarvest = 50,
      target = list("1" = 0.1), year = 2020L, verbose = 0),
    "Missing harvest targets")
})

test_that("a species with one available pixel cuts that pixel, not a random cell", {
  ## sample(x, 1) with a single x samples from 1:x, so pixel 8 became any of 1-8
  L <- toyLandscape()
  L$thlb[1:7] <- NA  # only pixel 8 is old enough and on the thlb
  cuts <- lapply(1:20, function(s) {
    set.seed(s)
    out <- harvestSpreadInputs(
      pixelGroupMap = L$pixelGroupMap, cohortData = L$cohortData, thlb = L$thlb,
      planningArea = L$planningArea, spreadProb = 1, maxCutSize = 2, minAgesToHarvest = 50,
      target = list("1" = 1), year = 2020L, verbose = 0)
    which(terra::values(out$rstCurrentHarvest)[, 1] == 1)
  })
  expect_identical(unique(unlist(cuts)), 8L)
})
