## The module's metadata is its contract with the projects that use it.

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_setequal(
    md$parameters$paramName,
    c(".plotInitialTime", ".plotInterval", ".plots", ".saveInitialTime", ".saveInterval",
      ".useCache", "harvestType", "hanzlik", "maxPatchSizetoHarvest", "minAgesToHarvest",
      "rotationAge", "spreadProb", "startTime", "verbose")
  )
})

test_that("harvest parameters have the documented defaults", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  p <- setNames(md$parameters$default, md$parameters$paramName)
  expect_identical(p$harvestType, "partial")
  expect_false(p$hanzlik)
  expect_true(is.na(p$rotationAge))
  expect_identical(p$minAgesToHarvest, 50)
})

test_that("outputs include what LandRCBM_partialDisturbance reads", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outs <- setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(outs[["rstCurrentHarvest"]], "SpatRaster")
  expect_identical(outs[["speciesHarvestMaps"]], "list")
  expect_identical(outs[["harvestSummary"]], "data.table")
})
