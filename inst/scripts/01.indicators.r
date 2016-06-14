
  # glue biological data sets together from various surveys
  p = list( project.name = "survey" )

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

  # load and glue data together
  survey.db( DS="set.init.redo", p=p )
  survey.db( DS="cat.init.redo", p=p )
  survey.db( DS="det.init.redo", p=p )

  # sanity checking and creation of new variables
  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  survey.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking

  # generic plots
  figure.bio.map.survey.locations()  # see mpa/src/_Rfunctions/figure.trawl.density for more control

    #  --- look in metabolism functions and complexity/condition

    # to obtain stats from l-w relationships used to impute mass/leng and estimate condition
    # a = length.weight.regression ( DS="parameters", p=p )

    # to obtain biomass estimates after correction for tow, etc.
    # a = biomass.estimation (DS="saved"", p=p )


  ######################################
  # all landings
  ######################################
  marfissci.get.data(save.csv=FALSE)
    # TODO:: create into a landings.db approach



  ######################################
  # survey data assimilation complete.
  # now, generate indicators of interest from survey data
  ######################################

  p = list(project.name = "indicators" )

  # -----------------------------
  # estimate condition
  p = bio.indicators::indicators.parameters( DS="condition", p=p)
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute

  # -----------------------------
  # estimate metabolic demand, given size structure
  p = bio.indicators::indicators.parameters( DS="metabolism", p=p)
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )


  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size
  p = bio.indicators::indicators.parameters( DS="sizespectrum", p=p)
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes


  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
  p = bio.indicators::indicators.parameters( DS="speciesarea", p=p)
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates


 # -----------------------------
   # ordination
  p = bio.indicators::indicators.parameters( DS="speciescomposition", p=p)
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )


 # -----------------------------
 # glue them all together

  p = list( project.name = "habitat" )
  p = bio.indicators::indicators.parameters( DS=p$project.name, p=p)

  # p$yearstomodel = 1970:2015  --- change this

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = c( rep( "nyx", 24), rep("tartarus", 24), rep("kaos", 24 ) )
  # p$clusters = rep("localhost", detectCores() )

  # 2. physical characteristics (depth, temp, substrate)
  # Time-invariant data (depth, substate, etc)
  #  baseline prediction surface in planar coords
  #  a glue function to bring in all available temperature and biological and substrate information
  #  This step needs to be completed after temperature db refresh

  ### ENSURE all following incoming data are up to date before running this:
  ### i.e. :
  ### loadfunctions( "bathymetry", functionname="bathymetry.r" )
  ### loadfunctions( "substrate", functionname="substrate.r" )
  ### loadfunctions( "groundfish", functionname="1.groundfish.r" )
  ### loadfunctions( "taxonomy", functionname="taxonomy.r" )
  ### loadfunctions( "temperature", functionname="temperature.r" )
  indicators.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc)
  lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format

  # 3. Contains all environmental data == baseline and temperature data ... none of the 'higher level indicators'
  # Used for merging back into survey.db as the 'higher level indicators have not yet been created/updated
  p = make.list( list( yrs=p$yearstomodel), Y=p )
  #parallel.run( indicators.db, DS="environmentals.redo", p=p ) #MG parallel isn't running properly at the moment
  indicators.db( DS="environmentals.redo", p=p )

