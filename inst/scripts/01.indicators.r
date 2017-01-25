
 
  current.year = 2016

  ## NOTE resolution is fixed at SSE

 # preform/prepare some lookup tables for faster lbm processing and generic lookups
  p = bio.indicators::indicators.parameters( DS="indicators" )
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

    p = bio.indicators::indicators.parameters( DS="survey" )
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
   

 # -----------------------------
  indicators.db( DS="prediction.surface.redo", p=p ) 

    if (0) {
      # not yet ready
      for ( vn in p$varstomodel) {
        print(vn)
        p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
        p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
        indicators.db ( DS="complete.redo", p=p )
        indicators.map( p=p  )
        gc()
      }
    }


  # to view progress in terminal:
  # watch -n 120 cat /home/jae/bio.data/bio.indicators/XXX/modelled/XXX/SSE/lbm_current_status

  # to view maps from an external R session:
  # lbm(p=p, tasks="debug_pred_static_map", vindex=1)
  # lbm(p=p, tasks="debug_pred_static_log_map", vindex=1)
  # lbm(p=p, tasks="debug_pred_dynamic_map", vindex=1)
  # lbm(p=p, tasks="debug_stats_map", vindex=1)


    #  --- look in metabolism functions and complexity/condition
    # to obtain stats from l-w relationships used to impute mass/leng and estimate condition
    # a = length.weight.regression ( DS="parameters", p=p )
    # to obtain biomass estimates after correction for tow, etc.
    # a = biomass.estimation (DS="saved"", p=p )

  p = make.list( list( yrs=p$yearstomodel), Y=p )
  #parallel.run( indicators.db, DS="environmentals.redo", p=p ) #MG parallel isn't running properly at the moment
  indicators.db( DS="environmentals.redo", p=p )


  # ----------------------------------------------------------
  # all landings

  p = bio.indicators::indicators.parameters( DS="landings" )
  bio.indicators::landings.db( DS="odbc", p=p ) # NOTE: run on MSWindows
  if (0) {
    # not yet ready
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }
  }


  # ----------------------------------------------------------
  # survey data assimilation complete.
  # now, generate indicators of interest from survey data


  # -----------------------------
  # ordination
  p = bio.indicators::indicators.parameters( DS="speciescomposition" )
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )
# o = bio.indicators::speciescomposition.db( DS="speciescomposition", p=p )
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }

  # ----------------------------------------------------------
  # estimate condition
  p = bio.indicators::indicators.parameters( DS="condition" )
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute
# o = bio.indicators::condition.db( DS="condition", p=p )
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



  # -----------------------------
  # estimate metabolic demand, given size structure
  p = bio.indicators::indicators.parameters( DS="metabolism")
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )
# o = bio.indicators::metabolism.db( DS="metabolism", p=p ) 
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size
  p = bio.indicators::indicators.parameters( DS="sizespectrum" )
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes
# o = bio.indicators::sizespectrum.db( DS="sizespectrum", p=p )
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
  p = bio.indicators::indicators.parameters( DS="speciesarea" )
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates
# o = bio.indicators::speciesarea.db( DS="speciesarea", p=p ) 
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }






 # -----------------------------
 # o = bio.indicators::biochem.db( DS="biochem", p=p )
    # for ( vn in p$varstomodel) {
    #   print(vn)
    #   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    #   p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
    #   indicators.db ( DS="complete.redo", p=p )
    #   indicators.map( p=p  )
    #   gc()
    # }





 # Used for merging back into survey.db as the 'higher level indicators have not yet been created/updated
  p = bio.indicators::indicators.parameters( DS="habitat" )
  indicators.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc)
  lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format

  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep("localhost", detectCores() )
  # p$clusters = c( rep( "nyx", 24), rep("tartarus", 24), rep("kaos", 24 ) )

