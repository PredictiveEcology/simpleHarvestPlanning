## Hanzlik: AAC = Vm / R + I; target = AAC / B old enough to harvest (capped at 1).
## Per pixel, group 1 has 5000 (age 80) + 1000 (60) + 400 (20); group 2 has 2000 (30) + 500 (10).

test_that("R = minAgesToHarvest = 50", {
  L <- toyLandscape()
  ## Vm = 8 x (5000 + 1000) = 48000
  ## I  = 8 x 400/20 + 8 x (2000/30 + 500/10) = 160 + 933.33 = 1093.33
  ## AAC = 48000 / 50 + 1093.33 = 2053.33; harvestable B = 48000
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb,
                       minAgesToHarvest = 50)
  expect_named(tgt, "1")
  expect_equal(tgt[["1"]], (48000 / 50 + 160 + 8 * (2000 / 30 + 50)) / 48000)
  expect_equal(round(tgt[["1"]], 5), 0.04278)
})

test_that("rotationAge separates R from the minimum harvest age", {
  L <- toyLandscape()
  ## R = 100: no cohort is mature, so Vm = 0 and every cohort adds B / age to I:
  ## I = 8 x (5000/80 + 1000/60 + 400/20) + 8 x (2000/30 + 500/10) = 793.33 + 933.33
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb,
                       minAgesToHarvest = 50, rotationAge = 100)
  expect_equal(round(tgt[["1"]], 5), round((793.3333 + 933.3333) / 48000, 5))
})

test_that("only the thlb counts", {
  L <- toyLandscape()
  L$thlb[9:16] <- NA  # group 2 is not harvestable land
  ## Vm = 48000, I = 160 only
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb,
                       minAgesToHarvest = 50)
  expect_equal(tgt[["1"]], (48000 / 50 + 160) / 48000)
})

test_that("each planningArea gets its own target", {
  L <- toyLandscape()
  L$planningArea[9:16] <- 2
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb,
                       minAgesToHarvest = 50)
  expect_named(tgt, c("1", "2"))
  expect_equal(tgt[["1"]], (48000 / 50 + 160) / 48000)
  expect_identical(tgt[["2"]], 0)  # nothing old enough to harvest
})

test_that("the target is capped at 1", {
  L <- toyLandscape()
  ## minimum harvest age above every cohort but one: harvestable B is only the age-80 cohort,
  ## while the increment of the rest is larger than it can supply
  L$cohortData[age == 80, B := 1L]
  tgt <- hanzlikTarget(L$cohortData, L$pixelGroupMap, L$planningArea, L$thlb,
                       minAgesToHarvest = 70, rotationAge = 200)
  expect_identical(tgt[["1"]], 1)
})
