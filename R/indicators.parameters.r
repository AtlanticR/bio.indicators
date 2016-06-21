

indicators.parameters = function( DS, p=NULL, current.year=NULL ) {

  if ( is.null(p) ) p=list()
  if ( !exists("project.name", p) ) p$project.name=DS
  if ( is.null(current.year)) current.year = lubridate::year(lubridate::now())

  if (DS=="survey"){
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )
    p$libs = c( p$libs, RLibrary ( "lubridate", "raster", "rgdal" ) )
    p$libs = c( p$libs, bioLibrary ( "bio.utilities", "bio.taxonomy", "bio.spacetime", "bio.habitat", "bio.indicators" ) )
    p$taxa =  "maxresolved"
    # p$seasons = "allseasons"
    p$data.sources = c("groundfish", "snowcrab")
    # habitat lookup parameters .. depth/temperature
    p$nw = 10 # number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64 ) # pseudo-log-scale
    p$default.spatial.domain = "canada.east"  # for temperature lookups
    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
    return(p)
  }


  # ---------------------

  if (DS=="landings"){
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )
    p$libs = c( p$libs, RLibrary ( "lubridate", "raster", "rgdal" ) )
    p$libs = c( p$libs, bioLibrary ( "bio.utilities", "bio.taxonomy", "bio.spacetime", "bio.habitat", "bio.indicators" ) )
    p$marfis.years=2002:current.year
    return(p)
  }


  # ---------------------

  if (DS=="condition") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory", "arm" , "snow" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )
    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) / 2 # half distances
    p$yearstomodel = 1970:current.year
    p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
                       "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
    p$spatial.knots = 100
    p$prediction.dyear = 0.75
    p$nw = 10
    p$default.spatial.domain = "canada.east"
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is default (newton), then this as a failsafe .. see GAM options
    # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
    p$modtype = "complex"
    return(p)
  }

  # ---------------------

  if (DS=="metabolism") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )
    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
    p$taxa = "alltaxa"   # do not use any other category
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
    p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
    p$yearstomodel = 1970:current.year
    p$habitat.predict.time.julian = "Sept-1" # Sept 1
    p$default.spatial.domain = "canada.east"
    p$prediction.dyear = 0.75
    p$spatial.knots = 100
    p$interpolation.distances =  25 # for interpolation of habitat vars
    p$nw = 10
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options
    p$modtype = "complex"
    return(p)
  }

  # ---------------------

  if (DS=="sizespectrum") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )
    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    p$use.bigmemory.file.backing = FALSE
    # p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
    p$taxa = "maxresolved"
    # p$taxa = "family.or.genera"
    # p$taxa = "alltaxa"
    p$season = "allseasons"
    # for spatial interpolation of nss stats
    p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
    p$yearstomodel = 1970:current.year
    p$modtype =  "complex"
    p$spatial.knots = 100
    p$prediction.dyear = 0.75
    p$nw = 10
    p$default.spatial.domain = "canada.east"

    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options

    p$timescale = c( 0,1,2,5 ) # yr
    p$interpolation.distances =  25 # for interpolation of habitat vars
    p$prediction.dyear = 0.75
    p$nw = 10

    # for generation of nss
    p$ntimescale = length(p$timescale)
    p$nss.distances=50  # km
    p$nss.stimes= 50 # days
    p$nss.type ="mass"
    p$nss.base =2
    p$nss.taxa = "all"

    if (p$nss.type=="mass") p$nss.bins = bins.df( "gf.mass", p$nss.base )
    if (p$nss.type=="len")  p$nss.bins = bins.df( "gf.len",  p$nss.base )
    return(p)
  }

  # ---------------------

  if (DS=="speciesarea") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$yearstomodel = 1970:current.year
    p$varstomodel = c( "C", "Z", "T", "Npred" )
    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    # file-backing is slower but can use all cpu's in a distributed cluster
    p$use.bigmemory.file.backing = FALSE
    p$clusters = rep("localhost", detectCores() )
    p$data.sources = c("groundfish", "snowcrab")
    p$speciesarea.method = "glm"
    p$pred.radius = 50 # km
    p$timescale = c( 0, 1, 2 ) # yr
    p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
    p$interpolation.distances = 25  # habitat interpolation scale
    p$taxa = "maxresolved" # p$taxa = "family.or.genera", "alltaxa"
    p$season = "allseasons"

    p$default.spatial.domain = "canada.east"
    p$modtype = "complex"
    p$prediction.dyear = 0.75 # =9/12 ie., 1 Sept
    p$nw = 10
    p$spatial.knots = 100
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options
    return(p)
  }

  # ---------------------

  if (DS=="speciescomposition") {

    p$project.outdir.root = project.datadirectory( "bio.indicators",  p$project.name ) #required for interpolations and mapping

    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
    p$data.sources = c("groundfish", "snowcrab")

    p$taxa = "maxresolved"
    p$season = "allseasons"
    p$timescale = c( 0,1,2,5,10 ) # yr
    p$interpolation.distances =  25 # for interpolation of habitat vars
    p$prediction.dyear = 0.75
    p$nw = 10

    p$yearstomodel = 1970:current.year
    p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )

    p$modtype = "complex"
    p$spatial.knots = 100
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton (default), then this .. see GAM options
    return(p)
  }

  # ---------------------

  if (DS=="habitat") {

    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory", "arm" , "snow" )
    p$libs = c( p$libs, bioLibrary ( "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$taxa = "maxresolved"
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
    p$interpolation.nmax = 100
    p$nw = 10  # from temperature.r, number of intervals in a year
    p$yearstomodel = 1970:current.year
    p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

    p$speciesarea.modeltype = "complex"
    p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
    p$speciesarea.season = "allseasons"
    p$speciesarea.taxa = "maxresolved"  # use only unique taxa
    p$speciesarea.data.sources = c("groundfish", "snowcrab")
    p$speciesarea.variables = c( "C", "Z", "T", "Npred" )

    p$speciescomposition.modeltype = "complex"
    p$speciescomposition.season = "allseasons"
    p$speciescomposition.taxa = "maxresolved"
    p$speciescomposition.variables = c( "ca1", "ca2" )

    p$sizespectrum.modeltype = "complex"
    p$sizespectrum.taxa = "maxresolved"
    p$sizespectrum.season = "allseasons"
    p$sizespectrum.variables = c( "nss.b1", "nss.rsquared", "nss.shannon")

    p$condition.modeltype = "complex"
    p$condition.taxa = "maxresolved"
    p$condition.season = "allseasons"
    p$condition.variables = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
                              "coSmallPelagic", "coLargePelagic", "coSmallDemersal", "coLargeDemersal")

    p$metabolism.modeltype = "complex"
    p$metabolism.taxa = "alltaxa"
    p$metabolism.season = "allseasons"
    p$metabolism.variables = c( "smr", "Pr.Reaction" , "Ea", "A", "qn", "qm", "mass", "len"  )
    return(p)
  }

  # ---------------------

  if (DS=="mpa") {

    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )

    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "rgdal", "INLA",
      "raster", "rasterVis", "parallel", "maps", "mapdata", "lattice"  )

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
    return(p)
  }

}
