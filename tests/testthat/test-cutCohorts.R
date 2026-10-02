## cutCohorts(): the cohorts removed in harvested pixels, i.e. the species set to 1 in
## speciesHarvestMaps for that pixel and aged minAgesToHarvest or more.

oneHarvested <- function(rtm, pix) {
  r <- terra::setValues(rtm, 0)
  r[pix] <- 1
  r
}

test_that("partial: only the species the pixel was cut under", {
  L <- toyLandscape()
  maps <- list(Pice_mar = oneHarvested(L$rasterToMatch, 1))
  cut <- cutCohorts(L$cohortData, L$pixelGroupMap, maps, minAgesToHarvest = 50)
  expect_identical(nrow(cut), 1L)
  expect_identical(as.character(cut$speciesCode), "Pice_mar")
  expect_identical(cut$pixelIndex, 1L)
  expect_identical(cut$B, 5000L)
})

test_that("clearcut: every species old enough, none of the young ones", {
  L <- toyLandscape()
  h <- oneHarvested(L$rasterToMatch, c(1, 2))
  spp <- levels(L$cohortData$speciesCode)
  maps <- setNames(rep(list(h), length(spp)), spp)
  cut <- cutCohorts(L$cohortData, L$pixelGroupMap, maps, minAgesToHarvest = 50)
  expect_setequal(as.character(cut$speciesCode), c("Pice_mar", "Betu_pap"))
  expect_identical(nrow(cut), 4L)              # 2 species x 2 pixels
  expect_true(all(cut$age >= 50))
  expect_identical(sum(cut$B), 2L * (5000L + 1000L))
})

test_that("a harvested pixel with nothing old enough gives no rows", {
  L <- toyLandscape()
  maps <- list(Pice_mar = oneHarvested(L$rasterToMatch, 9))  # group 2: Pice_mar is 30
  expect_identical(nrow(cutCohorts(L$cohortData, L$pixelGroupMap, maps, 50)), 0L)
})
