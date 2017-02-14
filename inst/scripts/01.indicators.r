
 
  ## NOTE resolution is fixed at SSE

  current.year = 2016

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )


  # ----------------------------------------------------------
  # preform/prepare some lookup tables for faster lbm processing and generic lookups
  
  for ( gr in c("SSE", "snowcrab") ) {
    p1 = spatial_parameters(p=p, type=gr )
    indicators.db( DS="spatial.redo", p=p1 ) 
    indicators.db( DS="spatial.annual.redo", p=p1 ) 
    # indicators.db( DS="spatial.annual.seasonal.redo", p=p1 ) 
    indicators.db( DS="prediction.surface.redo", p=p1 ) # used by all 02.*.r lbm processes and snowcrab_lbm()
  }

  
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
  survey.db( DS="lengthweight.redo", p=p  )  # # TODO:: parallelize me ... update the lcoal tables (not necessary)
  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  survey.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking

  figure.bio.map.survey.locations(p=p)  # see mpa/src/_Rfunctions/figure.trawl.density for more control
 
