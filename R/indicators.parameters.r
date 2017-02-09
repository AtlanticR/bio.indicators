

indicators.parameters = function( p=NULL, DS="default", current.year=NULL, varname=NULL ) {

  if ( is.null(p) ) p=list()

  if ( DS=="default") {

    p$libs = c( p$libs, RLibrary ( "lubridate", "rgdal", "parallel", "sp", "lattice", "fields", "mgcv" ) )
    p$libs = c( p$libs, bioLibrary (
      "bio.base", "bio.utilities", "bio.taxonomy", "bio.spacetime",  
      "bio.bathymetry", "bio.temperature", "bio.substrate", "bio.indicators") )

     
    p$spatial.domain = "SSE" 
    p$spatial.domain.subareas = c( "snowcrab")
    p = spatial_parameters( p )  # data are from this domain .. so far

    if (!exists( "current.year", p)) p$current.year = current.year
    
    p$yrs = c(1970:p$current.year)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    
    p$ny = length(p$yrs)
    p$nt = p$ny # must specify, else assumed = 1 (1= no time)  ## nt=ny annual time steps, nt = ny*nw is seassonal
    p$nw = 10 # default value of 10 time steps for all temp and indicators

    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = (c(1:p$nw)-1)  / p$nw # intervals of decimal years... fractional year breaks
    p$dyear_centre = p$dyears[ round(p$nw/2) ] + p$tres/2

    p$prediction.dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in indicators.parameters()

    # output timeslices for predictions in decimla years, yes all of them here
    p$prediction.ts = p$yrs + p$prediction.dyear 

    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )

  }



  # ---------------------


  if (DS=="survey"){
    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    
    p$taxa =  "maxresolved"
    p$data.sources = c("groundfish", "snowcrab")
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64 ) # pseudo-log-scale
    p$varstomodel = c()
  }


  # ---------------------

  if (DS=="landings"){
    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    p$marfis.years=2002:p$current.year
    p$varstomodel = c()
  }


  # ---------------------

  if (DS=="speciescomposition") {

    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    p$data.sources = c("groundfish", "snowcrab")
    p$taxa = "maxresolved"
    p$timescale = c( 0,1,2,5,10 ) # yr
    p$interpolation.distances =  25 # for interpolation of habitat vars
    p$yearstomodel = 1970:p$current.year
    # p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )
    p$varstomodel = c( "ca1", "ca2" )
    
    p$spatial.knots = 100

  }

  # ---------------------

  if (DS=="condition") {

    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) / 2 # half distances
    p$yearstomodel = 1970:p$current.year
    p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
                       "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
    p$spatial.knots = 100
    
  }


  # ---------------------

  if (DS=="metabolism") {

    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    p$taxa = "alltaxa"   # do not use any other category
    p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
    p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
    p$yearstomodel = 1970:p$current.year
    p$spatial.knots = 100
    p$interpolation.distances =  25 # for interpolation of habitat vars
  }

  # ---------------------

  if (DS=="sizespectrum") {

    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )
    p$libs = c( p$libs, RLibrary ( "bigmemory" ) )

    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    p$use.bigmemory.file.backing = FALSE  # for data assimilation, p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

    p$taxa = "maxresolved"
   
    # for spatial interpolation of nss stats
    # p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
    p$varstomodel = c( "nss.b0", "nss.b1", "nss.shannon" )
    
    p$yearstomodel = 1970:p$current.year
    p$modtype =  "complex"
    p$spatial.knots = 100

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

    p$project.name=DS    
    p$project.root = file.path( project.datadirectory( "bio.indicators"), p$project.name )

    p$libs = c( p$libs, RLibrary ( "bigmemory" ) )

    p$yearstomodel = 1970:p$current.year
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
    
    p$spatial.knots = 100

  }


  # ---------------------

  # if (DS=="indicators") {

  #   p$taxa = "maxresolved"
  #   p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )
  #   p$interpolation.nmax = 100
  #   p$nw = 10  # from temperature.r, number of intervals in a year
  #   p$yearstomodel = 1970:p$current.year

  #   p$speciesarea.modeltype = "complex"
  #   p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
  #   p$speciesarea.taxa = "maxresolved"  # use only unique taxa
  #   p$speciesarea.data.sources = c("groundfish", "snowcrab")
  #   p$speciesarea.variables = c( "C", "Z", "T", "Npred" )

  #   p$speciescomposition.modeltype = "complex"
  #   p$speciescomposition.taxa = "maxresolved"
  #   p$speciescomposition.variables = c( "ca1", "ca2" )

  #   p$sizespectrum.modeltype = "complex"
  #   p$sizespectrum.taxa = "maxresolved"
  #   p$sizespectrum.variables = c( "nss.b1", "nss.rsquared", "nss.shannon")

  #   p$condition.modeltype = "complex"
  #   p$condition.taxa = "maxresolved"
  #   p$condition.variables = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
  #                             "coSmallPelagic", "coLargePelagic", "coSmallDemersal", "coLargeDemersal")

  #   p$metabolism.modeltype = "complex"
  #   p$metabolism.taxa = "alltaxa"
  #   p$metabolism.variables = c( "smr", "Pr.Reaction" , "Ea", "A", "qn", "qm", "mass", "len"  )
  # }


  # ---------------------

  if (DS=="mpa") {

    p$project.name=DS    
    p$libs = c( p$libs, RLibrary ( "maps", "mapdata" ) )
    p$libs = c( p$libs,  bioLibrary( "bio.polygons", "netmensuration", "bio.spacetime", "bio.stomachs",
      "bio.coastline" ))

    p$spatial.domain = "SSE.mpa"  # override the default specified at top
    p = spatial_parameters( p )  # reset as SSE.mpa is a little larger

    p$taxa =  "maxresolved"
    p$data.sources = c("groundfish", "snowcrab")  # for survey.db

    p$map.regions = c("Canada", "USA") # library "map" coastline polygon designations
    p$map.output.directory = file.path( p$project.outdir.root, "maps")
    p$map.palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
    p$map.depthcontours = c( 200, 400, 600 ) # to plot on maps
    p$map.depthcontours.colours = c( "gray90", "gray85", "gray80", "gray74", "gray72", "gray70" )

    p$varstomodel = c()
  }


  # ----------------------

  if (DS=="lbm") {

    p$libs = RLibrary( c( p$libs, "lbm" ) ) # required for parallel processing
    if (!exists("storage.backend", p)) p$storage.backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )

    if (!exists("boundary", p)) p$boundary = FALSE 
    if (!exists("depth.filter", p)) p$depth.filter = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    if (!exists("lbm_quantile_bounds", p)) p$lbm_quantile_bounds = c(0.01, 0.99) # remove these extremes in interpolations
    
    if (!exists("lbm_rsquared_threshold", p)) p$lbm_rsquared_threshold = 0.1 # lower threshold
    if (!exists("lbm_distance_prediction", p)) p$lbm_distance_prediction = 7.5 # this is a half window km
    if (!exists("lbm_distance_statsgrid", p)) p$lbm_distance_statsgrid = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    if (!exists("lbm_distance_scale", p)) p$lbm_distance_scale = 50 # km ... approx guess of 95% AC range 
    if (!exists("lbm_distance_min", p)) p$lbm_distance_min = p$lbm_distance_statsgrid 
    if (!exists("lbm_distance_max", p)) p$lbm_distance_max = 75
  
    if (!exists("n.min", p)) p$n.min = 100 # n.min/n.max changes with resolution must be more than the number of knots/edf
    # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("n.max", p)) p$n.max = 8000 # no real upper bound

    if (!exists("sampling", p)) p$sampling = c( 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.1, 1.2, 1.5 )  # 

    if (!exists("variables", p)) p$variables = list( 
      Y = varname, 
      LOCS = c("plon", "plat"), 
      TIME = "tiyr", 
      COV = c("z", "dZ", "ddZ", "log.substrate.grainsize", "t", "tmean", "tamplitude" ) )
    p$varnames = c( p$variables$LOCS, p$variables$COV ) 
    
    if (!exists("lbm_variogram_method", p)) p$lbm_variogram_method = "fast"
    if (!exists("lbm_local_modelengine", p)) p$lbm_local_modelengine ="twostep"
    if (!exists("lbm_global_modelengine", p)) p$lbm_global_modelengine ="gam"

    # using covariates as a first pass essentially makes it ~ kriging with external drift
    if (!exists("lbm_global_modelformula", p))  p$lbm_global_modelformula = formula( paste( 
      varname, ' ~ s(yr) + s(dyear, k=3, bs="ts") + s(yr, dyear, k=36, bs="ts") ', 
      ' + s(t, bs="ts") + s(tmean, bs="ts") + s(tamplitude, bs="ts") + s(z, bs="ts")',
      ' + s(dZ, bs="ts") + s(ddZ, bs="ts")  + s(log.substrate.grainsize, bs="ts") ' )) 

    if (!exists("lbm_global_family", p)) p$lbm_global_family = gaussian()
    if (!exists("lbm_local_family", p)) p$lbm_local_family = gaussian()


    if (p$lbm_local_modelengine =="twostep") {

      p$lbm_local_modelformula = formula( paste(
        varname, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ', 
          ' + s(cos.w, sin.w, yr, bs="ts", k=36) ',
          ' + s(plon, bs="ts") + s(plat, bs="ts") + s(plon, plat, k=25, bs="ts") ' ) )
      p$lbm_local_model_distanceweighted = TRUE

      # p$lbm_twostep_space = "spatial.process"
      # p$lbm_twostep_space = "fft"
      # p$lbm_twostep_space = "tps"
      p$lbm_twostep_space = "krige"

    }  else if (p$lbm_local_modelengine == "gam") {

      p$lbm_local_modelformula = formula( paste(
        varname, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ', 
          ' + s(cos.w, sin.w, yr, bs="ts", k=36) ',
          ' + s(plon, bs="ts") + s(plat, bs="ts") + s(plon, plat, k=25, bs="ts") ' ) )    

      p$lbm_local_model_distanceweighted = TRUE
      p$lbm_gam_optimizer="perf"
      # p$lbm_gam_optimizer=c("outer", "bfgs") 
    
    }  else if (p$lbm_local_modelengine == "bayesx") {
 
      # bayesx families are specified as characters, this forces it to pass as is and 
      # then the next does the transformation internal to the "lbm__bayesx"
      p$lbm_local_family = "gaussian" 

      # alternative models .. testing .. problem is that SE of fit is not accessible?
      p$lbm_local_modelformula = formula( paste( 
        varname, ' ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps") +s(z, bs="ps") + sx(plon, bs="ps") + sx(plat,  bs="ps")', 
          ' + sx(plon, plat, cos.w, sin.w, yr, bs="te") ' )
          # te is tensor spline
      )
      p$lbm_local_model_bayesxmethod="MCMC"
      p$lbm_local_model_distanceweighted = FALSE
    
    } else {
    


      message( "The specified lbm_local_modelengine is not tested/supported ... you are on your own ;) ..." )

    }

  }

  return(p)

}


