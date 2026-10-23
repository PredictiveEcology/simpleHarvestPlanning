# simpleHarvestPlanning (development version)

* Harvest no longer lands outside the timber harvesting land base. When a species had only one pixel old enough to cut in a planning area, the cut started at a random cell instead of that pixel, often outside the land base or the study area. In a 100-year TSA02 run this misplaced 424 cuts, 306 of them off the land base. The pixel that was eligible was never cut.
* The Hanzlik target, rotation-age map and cut-cohort functions now come from the timberSupply package instead of copies inside the module. Results are unchanged.
* A harvest target can name only some species; the others are skipped instead of stopping the run.
* When no `planningArea` is supplied, the module makes one from `rasterToMatch` (the whole study area, split into as many areas as `harvestTarget` has). It used `thlb`, which does not exist yet when thlbBC makes it, so such a run stopped during setup.
* When no `thlb` is supplied, the one the module makes now leaves out land at 2000 m or higher, as intended, so it can be smaller than before. Its map is saved in the inputs folder, not a `GIS` folder in the working directory. The module also runs with only `rasterToMatch` and `thlb` supplied; the stand-ins for `cohortData` and `pixelGroupMap` stopped the run.

# simpleHarvestPlanning 0.1.0

simpleHarvestPlanning can now do partial cuts as well as clearcuts, and it sets the annual cut with the Hanzlik formula, based on the standing biomass and a rotation age. Spatial constraints can set different rotation ages for different areas, or mark areas where no harvest happens. New summary plots show the allowable cut against what was actually cut, biomass by species, and harvested area and age over time.

The harvest summary now records only the cohorts that were actually cut. The module gains tests, automatic checks and a rewritten manual.

Changes before 2026-09-01 are not included.
