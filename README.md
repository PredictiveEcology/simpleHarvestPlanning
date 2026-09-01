## simpleHarvestPlanning

This module is a very simple spatially explicit forest harvest planner made to run alongside LandR. You may supply it with one or more planning areas and harvest targets (species and percentages) or have the module determine harvest targets with the Hanzlik formula. Unless supplied by the user, the module assumes a single planning area covering the entire study area with a harvest target of 0.01 (1%).

Note that this module will only flag cohorts for harvest and will not actually harvest indicated cohorts. Another module must be used to remove the cohorts flagged by this simple module.

### List of parameters

Plotting and saving parameters:

-   `.plotInitialTime` - Defines when plotting begins, set to start of simulation by default.

-   `.plotInterval` - Defines plotting frequency.

-   `.saveInitialTime` - Defines when saving begins, set to start of simulation by default.

-   `.saveInterval` - Defines saving frequency.

-   `.useCache` - Defines whether or not the entire module should be run with caching. Off by default.

-   `verbose` - Determines level of detail in messaging during simulations, lower detail by default.

Harvest parameters:

-   `starTime` - Defines time step where initial harvest begins, set to start of simulation by default.

-   `minAgesToHarvest` - Defines minimum age trees can be harvested, default is set to 50

-   `maxPathSizeToHarvest` - Defines maximum size for harvestable patches, default is set to 10 pixels

-   `spreadProb` - Defines the spread probability when determining harvest patch size. 

-   `hanzlik` - Determines whether or not the Hanzlik formula is used to define harvest target, set to `FALSE` by default.
