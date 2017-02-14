

  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
 
  current.year = 2016
 

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciesarea" )
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates

# o = bio.indicators::speciesarea.db( DS="speciesarea", p=p ) 


  # -----------------------------
  # lbm; vn="smr" 
  for ( vn in p$varstomodel) {
    print(vn)

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="speciesarea"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   lbm( p=p, tasks=c( "stage0" ) ) # serial mode
    #   lbm( p=p, tasks=c( "continue" ) )    
    lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    lbm( p=p, tasks=c( "save" ) )
    
    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="speciesarea"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db ( p=p, DS="complete.redo" )
    indicators.db ( p=p, DS="baseline.redo" )
    indicators.map( p=p )

    if (0) {
      global_model = lbm_db( p=p, DS="global_model") 
      summary( global_model )
      plot(global_model)
    }
  }


