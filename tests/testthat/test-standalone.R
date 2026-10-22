## The module on its own: inputs it makes when no other module supplies them.

runStandalone <- function(objects, years = 1) {
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2020 + years - 1),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(.plots = NA, .plotInitialTime = NA)), moduleName),
    objects = objects
  )
  SpaDES.core::spades(sim, debug = FALSE)
}

test_that("with only rasterToMatch and thlb, the stand-in cohortData and pixelGroupMap run", {
  L <- toyLandscape()
  sim <- runStandalone(L[c("rasterToMatch", "thlb")], years = 2)
  expect_true(all(terra::values(sim$pixelGroupMap)[, 1] == 1L))
  expect_identical(nrow(sim$harvestSummary), 0L)  # the stand-in cohort is too young to cut
})

test_that("without thlb, it is made from the Managed Forests map, in inputPath", {
  skip_on_cran()
  skip_if_offline()
  L <- toyLandscape()
  ## a forested, tenured spot in the Southern Interior of BC
  for (n in c("rasterToMatch", "pixelGroupMap"))
    terra::ext(L[[n]]) <- terra::ext(-1880000, -1879000, 1060000, 1061000)
  withr::local_dir(withr::local_tempdir())
  sim <- runStandalone(L[c("rasterToMatch", "pixelGroupMap", "cohortData")])
  v <- terra::values(sim$thlb)[, 1]
  expect_true(all(v %in% c(1, NA)))
  expect_true(any(v %in% 1))
  expect_true(file.exists(file.path(testPaths$inputPath, "Canada_MFv2017.tif")))
  expect_false(dir.exists("GIS"))
})
