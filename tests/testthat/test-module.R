## The module in a simulation, with every input supplied so nothing is downloaded.

runToy <- function(harvestType, years = 2, hanzlik = TRUE, harvestTarget = list("1" = 0.1)) {
  L <- toyLandscape()
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2020 + years - 1),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(hanzlik = hanzlik, harvestType = harvestType, maxPatchSizetoHarvest = 2,
                                .plots = NA, .plotInitialTime = NA)), moduleName),
    objects = c(L[c("rasterToMatch", "pixelGroupMap", "cohortData", "thlb", "planningArea")],
                list(harvestTarget = harvestTarget))
  )
  SpaDES.core::spades(sim, debug = FALSE)
}

test_that("clearcut: every old cohort of a cut pixel is in harvestSummary", {
  set.seed(2)
  sim <- runToy("clearcut")
  hs <- sim$harvestSummary
  expect_gt(nrow(hs), 0)
  expect_true(all(hs$age >= 50))
  expect_setequal(unique(as.character(hs$speciesCode)), c("Pice_mar", "Betu_pap"))
  ## every species map is the harvest map itself
  for (m in sim$speciesHarvestMaps)
    expect_identical(terra::values(m), terra::values(sim$rstCurrentHarvest))
})

test_that("partial: only the dominant species is in harvestSummary", {
  set.seed(2)
  sim <- runToy("partial")
  hs <- sim$harvestSummary
  expect_gt(nrow(hs), 0)
  expect_identical(unique(as.character(hs$speciesCode)), "Pice_mar")
  expect_identical(hs[, data.table::uniqueN(speciesCode), by = .(year, pixelIndex)][, max(V1)], 1L)
})

test_that("an unknown harvestType is an error", {
  expect_error(runToy("selection", years = 1), "harvestType must be")
})

test_that("a year with nothing to cut runs and records nothing", {
  sim <- runToy("clearcut", hanzlik = FALSE, harvestTarget = list("1" = 0))
  expect_identical(nrow(sim$harvestSummary), 0L)
  expect_identical(sum(terra::values(sim$rstCurrentHarvest), na.rm = TRUE), 0)
})
