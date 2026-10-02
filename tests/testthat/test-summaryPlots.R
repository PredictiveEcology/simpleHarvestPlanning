## hanzlikStats and the summary time series / plots.

test_that("hanzlikTarget returns the parts of the AAC as a 'stats' attribute", {
  L <- toyLandscape()
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb, minAgesToHarvest = 50)
  st <- attr(tgt, "stats")
  expect_s3_class(st, "data.table")
  expect_identical(st$planningArea, 1)
  expect_equal(st$Vm, 48000)
  expect_equal(st$AAC, 48000 / 50 + st$I)
  expect_equal(st$target, tgt[["1"]])
})

test_that("a simulation records hanzlikStats each year, and the time series and plots build", {
  L <- toyLandscape()
  set.seed(2)
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2022),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(hanzlik = TRUE, harvestType = "clearcut", maxPatchSizetoHarvest = 2,
                                .plots = NA, .plotInitialTime = NA)), moduleName),
    objects = c(L[c("rasterToMatch", "pixelGroupMap", "cohortData", "thlb", "planningArea")],
                list(harvestTarget = list("1" = 0.1)))
  )
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_identical(sim$hanzlikStats$year, 2020:2022)

  ## a 250 m x 250 m pixel: B (g/m2) x 62500 m2 / 1e6 = t
  ts <- harvestTimeSeries(sim$harvestSummary, sim$hanzlikStats, sim$harvestStats, pixelArea = 62500)
  expect_setequal(unique(ts$aac$what), c("AAC (Hanzlik)", "Harvested"))
  expect_equal(ts$aac[what == "AAC (Hanzlik)"]$t, sim$hanzlikStats$AAC * 62500 / 1e6)
  expect_equal(sum(ts$species$t), sum(as.numeric(sim$harvestSummary$B)) * 62500 / 1e6)
  expect_setequal(unique(as.character(ts$area$what)), c("Expected", "Harvested"))
  expect_true(all(ts$age$mean >= 50))

  expect_s3_class(plotHarvestAAC(ts$aac), "ggplot")
  expect_s3_class(plotHarvestSpecies(ts$species), "ggplot")
  expect_s3_class(plotHarvestArea(ts$area), "ggplot")
  expect_s3_class(plotHarvestAge(ts$age), "ggplot")
  ## build them, which catches aesthetics that only fail when drawn
  for (gg in list(plotHarvestAAC(ts$aac), plotHarvestSpecies(ts$species),
                  plotHarvestArea(ts$area), plotHarvestAge(ts$age)))
    expect_no_error(ggplot2::ggplot_build(gg))
})

test_that("plotSummary writes the four figures when plotting is on", {
  L <- toyLandscape()
  set.seed(2)
  sim <- SpaDES.core::simInit(
    times = list(start = 2020, end = 2021),
    modules = moduleName,
    paths = testPaths,
    params = setNames(list(list(hanzlik = TRUE, harvestType = "clearcut", maxPatchSizetoHarvest = 2,
                                .plots = "png", .plotInitialTime = NA)), moduleName),
    objects = c(L[c("rasterToMatch", "pixelGroupMap", "cohortData", "thlb", "planningArea")],
                list(harvestTarget = list("1" = 0.1)))
  )
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  ## the module writes to figures/<module>; figurePath() outside an event is figures/
  pngs <- basename(list.files(SpaDES.core::outputPath(sim), "^harvest_.*png$", recursive = TRUE))
  expect_setequal(pngs, paste0(c("harvest_AAC_vs_cut", "harvest_biomass_by_species", "harvest_area",
                                 "harvest_age"), ".png"))
})
