

  # ----------------------------------------------------------
  # estimate condition

  current.year = 2016

 
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="condition" )
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute
# o = bio.indicators::condition.db( DS="condition", p=p )
 

  # -----------------------------
  # lbm; vn="condition.cod" ... 
  for ( vn in p$varstomodel) {
    print(vn)

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="condition"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   lbm( p=p, tasks=c( "stage0" ) ) # serial mode
    #   lbm( p=p, tasks=c( "continue" ) )    
    lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    lbm( p=p, tasks=c( "save" ) )
    
    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="condition"  )
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

