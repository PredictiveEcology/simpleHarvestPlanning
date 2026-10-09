## spatialConstraints: rotation ages by area; NA = no harvest, 0 = constraint does not apply.

test_that("rotationAgeMap: rotationAge where no constraint, no harvest first, then the longest rotation", {
  L <- toyLandscape()
  expect_identical(unique(terra::values(rotationAgeMap(NULL, L$rasterToMatch, 100))[, 1]), 100)
  protected <- planned <- terra::rast(L$rasterToMatch)
  protected[] <- 0
  planned[] <- 0
  protected[1:4] <- NA
  planned[3:8] <- 200
  rot <- terra::values(rotationAgeMap(c(protected, planned), L$rasterToMatch, 100))[, 1]
  expect_identical(rot, c(rep(NA, 4), rep(200, 4), rep(100, 8)))
})

test_that("NA pixels are never cut; each rotation age gets its own Hanzlik target", {
  L <- toyLandscape()
  sc <- terra::rast(L$rasterToMatch)
  sc[] <- 0
  sc[1:4] <- NA    # 4 of the 8 old pixels protected
  sc[5:6] <- 200   # 2 of them on a longer rotation
  set.seed(1)
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2024),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(hanzlik = TRUE, harvestType = "clearcut", maxPatchSizetoHarvest = 1,
                                verbose = 1, .plots = NA, .plotInitialTime = NA)), moduleName),
    objects = c(L[c("rasterToMatch", "pixelGroupMap", "cohortData", "thlb", "planningArea")],
                list(harvestTarget = list("1" = 0.1), spatialConstraints = sc))
  )
  ## one Hanzlik line per rotation age (R = 50 and R = 200), none for the protected pixels
  msgs <- capture_messages(sim <- SpaDES.core::spades(sim, debug = FALSE))
  hz <- grep("Hanzlik, planningArea", msgs, value = TRUE)
  expect_true(any(grepl("R = 50,", hz)))
  expect_true(any(grepl("R = 200,", hz)))
  expect_false(any(grepl("R = NA", hz)))
  expect_gt(nrow(sim$harvestSummary), 0)
  expect_false(any(sim$harvestSummary$pixelIndex %in% 1:4))
  expect_identical(sum(terra::values(sim$cumulativeHarvestMap)[1:4]), 0)
})

test_that("without Hanzlik, NA pixels are still never cut", {
  L <- toyLandscape()
  sc <- terra::rast(L$rasterToMatch)
  sc[] <- 0
  sc[1:4] <- NA
  set.seed(1)
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2022),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(hanzlik = FALSE, harvestType = "clearcut", maxPatchSizetoHarvest = 2,
                                .plots = NA, .plotInitialTime = NA)), moduleName),
    objects = c(L[c("rasterToMatch", "pixelGroupMap", "cohortData", "thlb", "planningArea")],
                list(harvestTarget = list("1" = 0.5), spatialConstraints = sc))
  )
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_gt(nrow(sim$harvestSummary), 0)
  expect_false(any(sim$harvestSummary$pixelIndex %in% 1:4))
})
