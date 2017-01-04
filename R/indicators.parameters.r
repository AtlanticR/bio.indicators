

indicators.parameters = function( DS, p=NULL, current.year=NULL ) {

  if ( is.null(p) ) p=list()
  if ( !exists("project.name", p) ) p$project.name=DS
  if ( is.null(current.year)) current.year = lubridate::year(lubridate::now())

  p$clusters = rep("localhost", detectCores() )

  p$prediction.dyear = 0.75
  p$spatial.domain = "SSE" # almost all expect "mpa"
  p$default.spatial.domain = "canada.east"  # for temperature lookups
  p = spatial_parameters( p )  # data are from this domain .. so far

  if (DS=="survey"){
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )
    p$libs = c( p$libs, RLibrary ( "lubridate", "raster", "rgdal" ) )
    p$libs = c( p$libs, bioLibrary ("bio.base", "bio.habitat", "bio.utilities", "bio.taxonomy", "bio.spacetime", "bio.habitat", "bio.indicators" ) )

    p$taxa =  "maxresolved"
    # p$seasons = "allseasons"
    p$data.sources = c("groundfish", "snowcrab")
    # habitat lookup parameters .. depth/temperature

    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64 ) # pseudo-log-scale
  

  }


  # ---------------------

  if (DS=="landings"){
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )
    p$libs = c( p$libs, RLibrary ( "lubridate", "raster", "rgdal" ) )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat", "bio.utilities", "bio.taxonomy", "bio.spacetime", "bio.habitat", "bio.indicators" ) )
    p$marfis.years=2002:current.year


  }


  # ---------------------

  if (DS=="condition") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory", "arm" , "snow" )

    p$libs = c( p$libs, bioLibrary ("bio.base", "bio.habitat",  "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) / 2 # half distances
    p$yearstomodel = 1970:current.year
    p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
                       "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
    
    p$spatial.knots = 100
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is default (newton), then this as a failsafe .. see GAM options
    # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
    p$modtype = "complex"


  }

  # ---------------------

  if (DS=="metabolism") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat", "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )


    p$taxa = "alltaxa"   # do not use any other category
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
    p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
    p$yearstomodel = 1970:current.year
    p$habitat.predict.time.julian = "Sept-1" # Sept 1
    p$spatial.knots = 100
    p$interpolation.distances =  25 # for interpolation of habitat vars
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options
    p$modtype = "complex"

  
  }

  # ---------------------

  if (DS=="sizespectrum") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat", "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )
    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    p$use.bigmemory.file.backing = FALSE
    # p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

    p$taxa = "maxresolved"
    # p$taxa = "family.or.genera"
    # p$taxa = "alltaxa"
    p$season = "allseasons"
    
    # for spatial interpolation of nss stats
    # p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
    p$varstomodel = c( "nss.b0", "nss.b1", "nss.shannon" )
    
    p$yearstomodel = 1970:current.year
    p$modtype =  "complex"
    p$spatial.knots = 100

    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options

    p$timescale = c( 0,1,2,5 ) # yr
    p$interpolation.distances =  25 # for interpolation of habitat vars

    # for generation of nss
    p$ntimescale = length(p$timescale)
    p$nss.distances=50  # km
    p$nss.stimes= 50 # days
    p$nss.type ="mass"
    p$nss.base =2
    p$nss.taxa = "all"

    if (p$nss.type=="mass") p$nss.bins = bins.df( "gf.mass", p$nss.base )
    if (p$nss.type=="len")  p$nss.bins = bins.df( "gf.len",  p$nss.base )

  
  }

  # ---------------------

  if (DS=="speciesarea") {
    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and mapping
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory" )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat", "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$yearstomodel = 1970:current.year
    p$varstomodel = c( "C", "Z", "T", "Npred" )

    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    # file-backing is slower but can use all cpu's in a distributed cluster
    p$use.bigmemory.file.backing = FALSE

    p$data.sources = c("groundfish", "snowcrab")
    p$speciesarea.method = "glm"
    p$pred.radius = 50 # km
    p$timescale = c( 0, 1, 2 ) # yr
    p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
    p$interpolation.distances = 25  # habitat interpolation scale
    p$taxa = "maxresolved" # p$taxa = "family.or.genera", "alltaxa"
    p$season = "allseasons"
    p$modtype = "complex"
    p$spatial.knots = 100
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options

  
  }

  # ---------------------

  if (DS=="speciescomposition") {

    p$project.outdir.root = project.datadirectory( "bio.indicators",  p$project.name ) #required for interpolations and mapping

    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal" )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat", "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$data.sources = c("groundfish", "snowcrab")

    p$taxa = "maxresolved"
    p$season = "allseasons"
    p$timescale = c( 0,1,2,5,10 ) # yr
    p$interpolation.distances =  25 # for interpolation of habitat vars

    p$yearstomodel = 1970:current.year
    # p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )
    p$varstomodel = c( "ca1", "ca2" )

    p$modtype = "complex"
    p$spatial.knots = 100
    p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton (default), then this .. see GAM options


    p$variables = list( Y="t", LOCS=c("plon", "plat"), TIME="tiyr", 
      COV=c("z", "dZ", "ddZ", "log.substrate.grainsize"), 
      COVT=c("t", "tmean", "tamp", "wmin" ) )
    p$varnames = c( p$variables$LOCS, p$variables$COV ) #

    if (!exists("lbm_variogram_method", p)) p$lbm_variogram_method = "fast"
    if (!exists("lbm_local_modelengine", p)) p$lbm_local_modelengine = "gam" # "twostep" might be interesting to follow up

    # using covariates as a first pass essentially makes it ~ kriging with external drift
    p$lbm_global_modelengine = NULL #"gam"
    p$lbm_global_modelformula = NULL # formula( t ~ s(z, bs="ts") ) # marginally useful .. consider removing it.
    p$lbm_global_family = gaussian()
  
    p$lbm_local_family = gaussian()

    if (p$lbm_local_modelengine =="gam") {
      # 32 hours on nyx all cpus; 
      # XX hrs on thoth all cpus
      
      p$lbm_local_modelformula = formula(
        t ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") + s( log(z), k=3, bs="ts")
          + s(plon,k=3, bs="ts") + s(plat, k=3, bs="ts")
          + s(plon, plat, cos.w, sin.w, yr, k=100, bs="ts") )  

formula( Yvar ~ as.factor(yr) + s(plon, plat, by=as.factor(yr), k=100, bs="tp" ) + s(dyear, k=3, bs="tp") + s(t, bs="tp" ) + s(tmean, bs="tp") + s(tamp, bs="tp" ) + s(wmin, bs="tp" ) + s(z, bs="tp" ) + s(dZ, bs="tp" ) + s(log.substrate.grainsize, bs="tp" ) ) 

      # more than 100 knots and it takes a very long time, 50 seems sufficient, given the large-scaled pattern outside of the prediction box
      # other possibilities:
        #     seasonal.basic = ' s(yr) + s(dyear, bs="cc") ',
        #     seasonal.smoothed = ' s(yr, dyear) + s(yr) + s(dyear, bs="cc")  ',
        #     seasonal.smoothed.depth.lonlat = ' s(yr, dyear) + s(yr, k=3) + s(dyear, bs="cc") +s(z) +s(plon) +s(plat) + s(plon, plat, by=yr), s(plon, plat, k=10, by=dyear ) ',
        p$lbm_local_model_distanceweighted = TRUE
        p$lbm_gam_optimizer="perf"
        # p$lbm_gam_optimizer=c("outer", "bfgs") 
    } else if (p$lbm_local_modelengine =="twostep") {
      # 34 hr with 8 CPU RAM on thoth, using 48 GB RAM .. about 1/3 faster than 24 cpus systems
      # 42 hrs on tartarus all cpus 
      # 18 GB RAM for 24 CPU .. 
      p$lbm_local_modelformula = formula(
        t ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") + s(log(z), k=3, bs="ts")
          + s(cos.w, sin.w, yr, bs="ts", k=9) )
        # similar to GAM model but no spatial component .. space is handled via FFT
      p$lbm_local_model_distanceweighted = TRUE

      p$lbm_fft_filter = "spatial.process"
      p$lbm_lowpass_phi = p$pres / 5 # FFT-baed methods cov range parameter .. not required for "spatial.process" ..
      p$lbm_lowpass_nu = 0.5

    } else if (p$lbm_local_modelengine =="spate") {
 
      # similar to the two-step but use "spate" (spde, bayesian, mcmc) instead of "fields" (GMRF, ML)
      p$lbm_local_modelformula = formula(
        t ~ s(yr, k=5, bs="ts") + s(cos.w, bs="ts") + s(sin.w, bs="ts") + s( log(z), k=3, bs="ts")
          + s(cos.w, sin.w, yr, bs="ts") )
        # similar to GAM model but no spatial component , space and time are handled via FFT but time is seeded by the averge local TS signal (to avoid missing data isses in time.)
      p$lbm_local_model_distanceweighted = TRUE
 
    } else if (p$lbm_local_modelengine == "bayesx") {
 
      # bayesx families are specified as characters, this forces it to pass as is and 
      # then the next does the transformation internal to the "lbm__bayesx"
      p$lbm_local_family_bayesx = "gaussian" 

      # alternative models .. testing .. problem is that SE of fit is not accessible?
      p$lbm_local_modelformula = formula(
        t ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps") +s(z, bs="ps")
          + sx(plon, bs="ps") + sx(plat,  bs="ps")
          + sx(plon, plat, cos.w, sin.w, yr, bs="te")  # te is tensor spline
      )
      p$lbm_local_model_bayesxmethod="MCMC"
      p$lbm_local_model_distanceweighted = FALSE
    
    } else {
    
      message( "The specified lbm_local_modelengine is not tested/supported ... you are on your own ;) ..." )

    }
   



  
  }

  # ---------------------

  if (DS=="habitat") {

    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name ) #required for interpolations and
    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal", "bigmemory", "arm" , "snow" )
    p$libs = c( p$libs, bioLibrary ( "bio.base", "bio.habitat","bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators", "bio.taxonomy" ) )

    p$taxa = "maxresolved"
    p$season = "allseasons"
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
    p$interpolation.nmax = 100
    p$nw = 10  # from temperature.r, number of intervals in a year
    p$yearstomodel = 1970:current.year

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

  
  }

  # ---------------------

  if (DS=="mpa") {

    p$project.outdir.root = project.datadirectory( "bio.indicators", p$project.name )

    p$libs = RLibrary ( "lubridate", "fields", "mgcv", "sp", "parallel", "rgdal", "INLA",
      "raster", "rasterVis", "parallel", "maps", "mapdata", "lattice"  )

    p$libs = c( p$libs,  bioLibrary(
      "bio.base", "bio.utilities", "bio.groundfish", "bio.snowcrab", "bio.plankton", "bio.remote.sensing", "bio.habitat", "bio.taxonomy",
      "bio.bathymetry", "bio.substrate", "bio.temperature", "bio.polygons", "netmensuration", "bio.spacetime", "bio.stomachs",
      "bio.coastline", "bio.indicators" ))

    p$spatial.domain = "SSE.mpa"  # override the default specified at top
    p = spatial_parameters( p )  # reset as SSE.mpa is a little larger

    p$taxa =  "maxresolved"
    p$seasons = "allseasons"
    p$data.sources = c("groundfish", "snowcrab")  # for survey.db

    p$map.regions = c("Canada", "USA") # library "map" coastline polygon designations
    p$map.output.directory = file.path( p$project.outdir.root, "maps")
    p$map.palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
    p$map.depthcontours = c( 200, 400, 600 ) # to plot on maps
    p$map.depthcontours.colours = c( "gray90", "gray85", "gray80", "gray74", "gray72", "gray70" )

  
  }


  if (DS=="lbm") {

    p$libs = RLibrary( c( p$libs, "lbm" ) ) # required for parallel processing
    p$storage.backend="bigmemory.ram"

    p$boundary = TRUE 
    p$depth.filter = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    p$lbm_nonconvexhull_alpha = 20  # radius in distance units (km) to use for determining boundaries
    p$lbm_noise = 0.001  # distance units for eps noise to permit mesh gen for boundaries
    p$lbm_quantile_bounds = c(0.01, 0.99) # remove these extremes in interpolations
    
    p$lbm_rsquared_threshold = 0.25 # lower threshold
    p$lbm_distance_prediction = 7.5 # this is a half window km
    p$lbm_distance_statsgrid = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    p$lbm_distance_scale = 25 # km ... approx guess of 95% AC range 
    p$lbm_distance_min = p$lbm_distance_statsgrid 
    p$lbm_distance_max = 50 

  
    p$n.min = 200 # n.min/n.max changes with resolution must be more than the number of knots/edf
    # min number of data points req before attempting to model timeseries in a localized space
    p$n.max = 2000 # numerical time/memory constraint -- anything larger takes too much time
    p$sampling = c( 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.1, 1.2, 1.5, 1.75, 2 )  # 

  }

  return(p)

}


