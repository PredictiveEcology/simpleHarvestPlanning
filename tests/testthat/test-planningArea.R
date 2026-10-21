## defaultPlanningArea(): the planningArea made when none is supplied. It is built from
## rasterToMatch, because thlb may only exist after thlbBC's init has run.

test_that("one planning area covers every pixel of the study area", {
  rtm <- toyLandscape()$rasterToMatch
  rtm[c(1, 16)] <- NA  # outside the study area
  pa <- defaultPlanningArea(rtm, 1)
  expect_true(terra::compareGeom(pa, rtm))
  inside <- !is.na(terra::values(rtm)[, 1])
  expect_true(all(terra::values(pa)[inside, 1] == 1))
  expect_true(all(is.na(terra::values(pa)[!inside, 1])))
})

test_that("several planning areas split the study area into numbered parts", {
  L <- toyLandscape()
  pa <- defaultPlanningArea(L$rasterToMatch, 3)
  v <- terra::values(pa)[, 1]
  expect_setequal(unique(v[!is.na(v)]), 1:3)
  expect_identical(sum(!is.na(v)), sum(!is.na(terra::values(L$rasterToMatch)[, 1])))
})
