
p = list( project.name="bio.indicators" )

p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

p$libs = RLibrary ( c(
  "lubridate", "fields", "mgcv", "sp", "parallel", "rgdal", "INLA",
  "raster", "rasterVis", "parallel", "maps", "mapdata", "lattice"  ))

p$libs = c( p$libs,  bioLibrary(
  "bio.utilities", "bio.groundfish", "bio.snowcrab", "bio.plankton", "bio.remote.sensing", "bio.habitat", "bio.taxonomy",
  "bio.bathymetry", "bio.substrate", "bio.temperature", "bio.polygons", "netmensuration", "bio.spacetime", "bio.stomachs",
  "bio.coastline", "bio.indicators" ))

p = spatial.parameters( p, "SSE.mpa" )

p$default.spatial.domain = "canada.east"  # for temperature lookups
p$taxa =  "maxresolved"
p$seasons = "allseasons"
p$data.sources = c("groundfish", "snowcrab")  # for survey.db
p$nw = 10 # for lookup of temperature: number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )

p$map.regions = c("Canada", "USA") # library "map" coastline polygon designations
p$map.output.directory = file.path( p$project.outdir.root, "maps")
p$map.palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
p$map.depthcontours = c( 200, 400, 600 ) # to plot on maps
p$map.depthcontours.colours = c( "gray90", "gray85", "gray80", "gray74", "gray72", "gray70" )


p$libs = bioLibrary( "bio.indicators" )

marfissci.get.data(save.csv=FALSE)

