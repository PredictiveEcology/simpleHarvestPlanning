---
title: "simpleHarvestPlanning Manual"
date: "Last updated: 2026-10-02"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    toc_depth: 3
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
always_allow_html: true
---



**Authors:** Ian Eddy, Parvin Kalantari. Module and documentation are work in progress.

# Overview

*simpleHarvestPlanning* is a simple, spatially explicit harvest planner for LandR. Each year it
chooses which pixels to harvest and which species to cut in them, and records what that removes.

It **does not remove anything itself**. Pair it with *LandRCBM_partialDisturbance*, which reads
`speciesHarvestMaps`, removes the flagged cohorts from `cohortData` and replants the pixels.

What it decides:

- **How much** to cut in each planning area: a fixed share per year, or Hanzlik's annual
  allowable cut (see [Harvest targets](#targets)), with other rotation ages, or none, where
  `spatialConstraints` apply (see [Spatial constraints](#constraints)).
- **Where** to cut: stands old enough, on the timber harvesting land base (THLB), grouped into
  patches (see [How a harvest year works](#selection)).
- **What** to cut in a harvested pixel: the dominant species only, or a clearcut of every species
  (see [Harvest type](#harvest-type)).

# How a harvest year works {#selection}

The `harvest` event runs every year from `startTime`.

1. If `hanzlik = TRUE`, the target for each planning area is recalculated from the current
   landscape (see [Harvest targets](#targets)). Otherwise the `harvestTarget` input is used as given.
2. Each pixel gets a stand age, the biomass-weighted mean age of its cohorts, and a dominant
   species, the one with the most biomass. A pixel is eligible if it is on the THLB and its stand
   age is at least `minAgesToHarvest`.
3. Within each planning area, eligible pixels are grouped by dominant species. For each species
   the module cuts `ceiling(target x eligible pixels)`. It does this by seeding patches and growing
   them with `SpaDES.tools::spread2()`, up to `maxPatchSizetoHarvest` pixels each, with `spreadProb`
   controlling how close patches get to that size.
4. The harvest is written out (see [Outputs](#outputs)).

# Harvest targets {#targets}

A target is the share of eligible pixels to cut each year in a planning area. It can also be set
per species.

## Fixed targets

With `hanzlik = FALSE` (the default), `harvestTarget` is used as given. It is a list named by
planning area. Each element is either one number or a list of numbers by species, with
`default` for the species not listed:


``` r
harvestTarget <- list(
  "1" = list(default = 0.005, Pinu_con = 0.01, Pice_gla = 0.02),
  "2" = list(default = 0.02, Pinu_con = 0.01),
  "3" = 0.01,   # every species in planning area 3
  "4" = 0.008
)
```

If `harvestTarget` and `planningArea` are not supplied, the module uses one planning area with a
target of 0.01.

## Hanzlik's annual allowable cut

With `hanzlik = TRUE`, each year and for each planning area the module computes Hanzlik's annual
allowable cut (AAC) from the current cohorts on the THLB:

$$\text{AAC} = \frac{V_m}{R} + I$$

- $V_m$ is the mature biomass: the B of all cohorts aged $R$ or more.
- $R$ is the rotation age, the `rotationAge` parameter. If that is `NA` (the default),
  `minAgesToHarvest` is used.
- $I$ is the mean annual increment of the younger stands: the sum of $B / \text{age}$ over cohorts
  younger than $R$.

The first term spreads the mature biomass over one rotation. The second adds what the young
forest grows each year.

Biomass stands in for volume: B is in g/m², summed over pixels. Because the module cuts a share of
pixels, the AAC is turned into a target by dividing it by the biomass old enough to harvest
(cohorts aged `minAgesToHarvest` or more), capped at 1. On average the biomass of the pixels cut
then equals the AAC. With `verbose = 1` the module prints $V_m$, $R$, $I$, the AAC and the target
every year.

Setting `rotationAge` separately matters. Boreal rotations are usually 80--120 years, much longer
than the default minimum harvest age of 50. Using 50 for $R$ makes $V_m / R$ large.

## Spatial constraints {#constraints}

`spatialConstraints` (optional) sets other rotation ages in parts of the landscape, e.g. protected
areas or planned protected areas. It is a `SpatRaster` with one layer per constraint. Each layer
holds that constraint's rotation age where it applies, `NA` for no harvest, and `0` where it does
not apply.


``` r
protected <- plannedProtected <- terra::rast(rasterToMatch)
protected[] <- 0                      # 0: this constraint does not apply
plannedProtected[] <- 0
protected[parkPixels] <- NA           # never harvested
plannedProtected[plannedPixels] <- 200
spatialConstraints <- c(protected = protected, plannedProtected = plannedProtected)
```

Where layers overlap, no harvest wins, then the longest rotation. Pixels in no constraint use
`rotationAge`.

- `NA` pixels are removed from the THLB, with or without Hanzlik.
- With `hanzlik = TRUE`, each rotation age in a planning area is a separate harvest: its own
  $V_m$, $I$, AAC and target, and its own pixel selection. A longer rotation is cut at its own,
  lower, rate. With `verbose = 1` there is one Hanzlik line per rotation age.

`harvestStats` and `harvestPerformance` then have one set of rows per rotation age.

# Harvest type {#harvest-type}

`harvestType` sets what is cut in a harvested pixel. In both cases only cohorts aged
`minAgesToHarvest` or more are cut.

| `harvestType` | What is cut | `speciesHarvestMaps` |
|---|---|---|
| `"partial"` (default) | Only the dominant species the pixel was selected under. Other species stay, so a mixed stand can be selected again in a later year for another species. | One raster per species, holding only the pixels selected under that species. |
| `"clearcut"` | Every species. | One raster per species, each equal to `rstCurrentHarvest`. |

*LandRCBM_partialDisturbance* removes, for each species, the cohorts in that species' raster, so
it needs no setting of its own. Two things to know about that pairing:

- It removes cohorts aged 50 to 500 by its own default, which matches `minAgesToHarvest` only at
  the default of 50.
- It runs before the harvest event in a year, so cohorts flagged in year *t* are removed in year
  *t + 1*.

# Outputs {#outputs}

- `rstCurrentHarvest`: this year's harvest, 1 = harvested, 0 = not, `NA` = non-forest.
- `speciesHarvestMaps`: the species to cut in each harvested pixel (see [Harvest type](#harvest-type)).
- `harvestSummary`: one row per year, pixel and cohort that is cut, with `speciesCode`, `age`,
  `B` and `planningArea`. Only the cohorts that are cut are listed, not every cohort in a harvested
  pixel, so `sum(B)` is the harvested biomass. Values are at the time of selection, one year before
  removal.
- `cumulativeHarvestMap`: how many times each pixel has been harvested.
- `timeSinceHarvest`: years since each pixel was last harvested (`NA` = never).
- `harvestStats`, `harvestPerformance`: per year, planning area and species, how many pixels were
  expected and how many were cut.

If nothing can be cut in a year (a target of 0, or nothing old enough), the year runs and records
nothing.

# Parameters


|paramName             |paramClass |default |min  |max |paramDesc                                                                                                                                                                                                                                                                     |
|:---------------------|:----------|:-------|:----|:---|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime      |numeric    |0       |NA   |NA  |This is here for backwards compatibility. Please use `.plots`                                                                                                                                                                                                                 |
|.plots                |character  |png     |NA   |NA  |This describes the type of 'plotting' to do. See `?Plots` for possible types. To omit, set to NA                                                                                                                                                                              |
|.plotInterval         |numeric    |NA      |NA   |NA  |This describes the simulation time interval between plot events                                                                                                                                                                                                               |
|.saveInitialTime      |numeric    |NA      |NA   |NA  |This describes the simulation time at which the first save event should occur                                                                                                                                                                                                 |
|.saveInterval         |numeric    |NA      |NA   |NA  |This describes the simulation time interval between save events                                                                                                                                                                                                               |
|.useCache             |logical    |FALSE   |NA   |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant                                                                                                                      |
|startTime             |numeric    |0       |NA   |NA  |Simulation time at which to initiate harvesting                                                                                                                                                                                                                               |
|minAgesToHarvest      |numeric    |50      |1    |NA  |minimum ages of trees to harvest                                                                                                                                                                                                                                              |
|maxPatchSizetoHarvest |numeric    |10      |1    |NA  |maximum size for harvestable patches, in pixels                                                                                                                                                                                                                               |
|spreadProb            |numeric    |1       |0.01 |1   |spread prob when determing harvest patch size. Larger spreadProb yields cuts closer to max. Exceeding 1 will likely result in harvest patches that are maximum size                                                                                                           |
|verbose               |numeric    |0       |0    |1   |if 1, print more detailed messaging about harvest                                                                                                                                                                                                                             |
|hanzlik               |logical    |FALSE   |NA   |NA  |toggles whether or not the Hanzlik formula is used to determine harvest: annual cut = Vm / R + I, in biomass, per planningArea on the thlb. Vm = biomass of cohorts aged `rotationAge` or more, R = `rotationAge`, I = mean annual increment (B / age) of younger cohorts.    |
|rotationAge           |numeric    |NA      |1    |NA  |Rotation age (R) in the Hanzlik formula. NA uses `minAgesToHarvest`. `spatialConstraints` override it where they apply.                                                                                                                                                       |
|harvestType           |character  |partial |NA   |NA  |What is cut in a harvested pixel. 'partial': only the cohorts of the species the pixel was selected under (its dominant species). 'clearcut': the cohorts of every species. Either way, only cohorts aged `minAgesToHarvest` or more. Expressed through `speciesHarvestMaps`. |

# Inputs


|objectName           |objectClass |desc                                                                                                                                                                                                                                                                                                                                                                                                                  |sourceURL |
|:--------------------|:-----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|planningArea         |SpatRaster  |Raster of planning area ids                                                                                                                                                                                                                                                                                                                                                                                           |NA        |
|harvestTarget        |list        |List containing the harvest targets for each planningArea. Can optionally be defined per species in a planningArea                                                                                                                                                                                                                                                                                                    |NA        |
|cohortData           |data.table  |table with pixelGroup, age, species, and biomass of cohorts                                                                                                                                                                                                                                                                                                                                                           |NA        |
|cumulativeHarvestMap |SpatRaster  |cumulative harvest in raster form                                                                                                                                                                                                                                                                                                                                                                                     |NA        |
|pixelGroupMap        |SpatRaster  |Raster of pixelGroup locations                                                                                                                                                                                                                                                                                                                                                                                        |NA        |
|rasterToMatch        |SpatRaster  |Template raster                                                                                                                                                                                                                                                                                                                                                                                                       |NA        |
|studyArea            |SpatVector  |Study area polygon                                                                                                                                                                                                                                                                                                                                                                                                    |NA        |
|thlb                 |SpatRaster  |Harvestable pixels mask                                                                                                                                                                                                                                                                                                                                                                                               |NA        |
|spatialConstraints   |SpatRaster  |Optional. One layer per constraint (e.g. protected, plannedProtected), holding the rotation age that applies there, NA = no harvest, and 0 where it does not apply. Where layers overlap the longest rotation wins; elsewhere `rotationAge` applies. NA pixels are never harvested; with `hanzlik = TRUE`, each rotation age gets its own AAC, target and pixel selection. If not supplied, there are no constraints. |NA        |
|timeSinceHarvest     |SpatRaster  |map of time since last harvest; new harvests start at 0                                                                                                                                                                                                                                                                                                                                                               |NA        |

`rasterToMatch` is required. When missing, the others are created:

- `thlb` from the Managed Forests of Canada map (2017): long- and short-term tenure and private
  forest (classes 11, 12, 50) are harvestable.
- `planningArea` as one area over the THLB, or as equal splits when `harvestTarget` has several
  areas.
- `harvestTarget` as `list("1" = 0.01)`.
- `timeSinceHarvest` as all `NA`.

# Output objects


|objectName           |objectClass |desc                                                                                                                                         |
|:--------------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------|
|rstCurrentHarvest    |SpatRaster  |Binary raster representing with 1 representing harvested pixels and 0 non-harvested forest. NA values represent non-forest                   |
|cumulativeHarvestMap |SpatRaster  |cumulative harvest in raster form                                                                                                            |
|harvestSummary       |data.table  |data.table with year and pixel index of harvested pixels                                                                                     |
|harvestStats         |data.table  |data.table with storage for minCuts, totalCut, and target                                                                                    |
|speciesHarvestMaps   |list        |List of binary SpatRasters representing harvested pixels per species. Each raster has 1 for harvested pixels and 0 for non-harvested pixels. |
|harvestPerformance   |list        |List with observed vs expected harvest summaries per year and per planning area                                                              |
|thlb                 |SpatRaster  |Harvestable pixels mask                                                                                                                      |

# Usage

A clearcut with a Hanzlik cut and a 100-year rotation, alongside the LandR biomass modules:


``` r
out <- SpaDES.project::setupProject(
  modules = c("PredictiveEcology/Biomass_speciesData@development",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_core@development",
              "PredictiveEcology/simpleHarvestPlanning@development",
              "camillegiuliano/LandRCBM_partialDisturbance@main"),
  times = list(start = 2020, end = 2030),
  params = list(
    simpleHarvestPlanning = list(hanzlik = TRUE, rotationAge = 100,
                                 harvestType = "clearcut", verbose = 1)
  ),
  studyArea = studyArea  # an sf polygon
)
sim <- SpaDES.core::simInitAndSpades2(out)
sim$harvestSummary[, .(pixels = data.table::uniqueN(pixelIndex), B = sum(B)), by = year]
```

# Tests

`tests/testthat/` holds unit tests for the target, selection and summary functions, and short
simulations on a 4 x 4 landscape with every input supplied, so nothing is downloaded. The
`testthat-module` GitHub workflow runs them on every pull request, using the shared workflow in
[PredictiveEcology/actions](https://github.com/PredictiveEcology/actions). To run them locally
from the module directory, do what that workflow does:


``` r
pkg <- SpaDES.core::convertToPackage("simpleHarvestPlanning", path = "..",
                                     buildDocuments = TRUE,
                                     destinationPath = tempfile("testthat-module"))
testthat::test_local(pkg)
```
