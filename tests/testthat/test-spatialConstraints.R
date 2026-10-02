## spatialConstraints: rotation ages by area; Inf = no harvest.

test_that("rotationAgeMap: rotationAge where no constraint, the longest rotation where they overlap", {
  L <- toyLandscape()
  expect_identical(unique(terra::values(rotationAgeMap(NULL, L$rasterToMatch, 100))[, 1]), 100)
  protected <- planned <- terra::rast(L$rasterToMatch)
  protected[] <- NA
  planned[] <- NA
  protected[1:4] <- Inf
  planned[3:8] <- 200
  rot <- terra::values(rotationAgeMap(c(protected, planned), L$rasterToMatch, 100))[, 1]
  expect_identical(rot, c(rep(Inf, 4), rep(200, 4), rep(100, 8)))
})

test_that("Inf pixels are never cut; each rotation age gets its own Hanzlik target", {
  L <- toyLandscape()
  sc <- terra::rast(L$rasterToMatch)
  sc[] <- NA
  sc[1:4] <- Inf   # 4 of the 8 old pixels protected
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
  ## one Hanzlik line per finite rotation age (R = 50 and R = 200), none for Inf
  msgs <- capture_messages(sim <- SpaDES.core::spades(sim, debug = FALSE))
  hz <- grep("Hanzlik, planningArea", msgs, value = TRUE)
  expect_true(any(grepl("R = 50,", hz)))
  expect_true(any(grepl("R = 200,", hz)))
  expect_false(any(grepl("R = Inf", hz)))
  expect_gt(nrow(sim$harvestSummary), 0)
  expect_false(any(sim$harvestSummary$pixelIndex %in% 1:4))
  expect_identical(sum(terra::values(sim$cumulativeHarvestMap)[1:4]), 0)
})

test_that("without Hanzlik, Inf pixels are still never cut", {
  L <- toyLandscape()
  sc <- terra::rast(L$rasterToMatch)
  sc[] <- NA
  sc[1:4] <- Inf
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
