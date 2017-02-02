
 
  ## NOTE resolution is fixed at SSE

  current.year = 2016

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )


  # ----------------------------------------------------------
  # preform/prepare some lookup tables for faster lbm processing and generic lookups
  indicators.db( DS="spatial.redo", p=p ) 
  indicators.db( DS="spatial.annual.redo", p=p ) 
  indicators.db( DS="spatial.annual.seasonal.redo", p=p ) 

  
  # ----------------------------------------------------------
  # glue biological data sets together from various surveys

  # load and glue data together
  if (redo.source.data) {
    # these are here to show the dependencies of survey.db()
    bio.groundfish::groundfish.db( "set.base.redo" )
    bio.groundfish::groundfish.db( "cat.base.redo" ) 
    bio.groundfish::groundfish.db( "det.redo" )
    bio.snowcrab::snowcrab.db( DS ="set.clean.redo" )
    bio.snowcrab::snowcrab.db( DS ="cat.georeferenced.redo" ) 
    bio.snowcrab::snowcrab.db( DS ="det.georeferenced.redo" )
  }

  p = bio.indicators::indicators.parameters( p=p, DS="survey" )
  survey.db( DS="set.init.redo", p=p )
  survey.db( DS="cat.init.redo", p=p )
  survey.db( DS="det.init.redo", p=p )

  # the following requires the preformed indicators.db() (above) for lookups to complete the data
  survey.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  survey.db( DS="length.weight.redo", p=p  )  # # TODO:: parallelize me ... update the lcoal tables (not necessary)
  survey.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking

  figure.bio.map.survey.locations(p=p)  # see mpa/src/_Rfunctions/figure.trawl.density for more control
 
  indicators.db( DS="prediction.surface.redo", p=p ) 


 # Used for merging back into survey.db as the 'higher level indicators have not yet been created/updated
  p = bio.indicators::indicators.parameters( DS="habitat" )
  indicators.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc)
  lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format

  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep("localhost", detectCores() )
  # p$clusters = c( rep( "nyx", 24), rep("tartarus", 24), rep("kaos", 24 ) )

