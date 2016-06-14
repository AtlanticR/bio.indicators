This package operates to assimilate various fishery surveys and environmental data sets. It accesses them at a low level where possible and computes various indicators to summarise system state. Interpolation methods are enabled to permit smooth fields for other interpolations and/or further study. This R package is designed to work with other beothuk/bio.* libraries.

.libPaths()/bio.indicators/scripts/01.indicators.r  # assimilates and creates the indicators
.libPaths()/bio.indicators/scripts/02.interpolations.r # interpolates the indicators of interest in space and time
.libPaths()/bio.indicators/scripts/03.indicators.ts.r  # assimilates other indicators in timeseries, used variously in snowcrab assessments
.libPaths()/bio.indicators/scripts/10.mpa.r  # integrates indicators used in MPA assessments


