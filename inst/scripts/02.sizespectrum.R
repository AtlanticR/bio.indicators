
  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size

  current.year = 2016

  
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="sizespectrum" )
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes
# o = bio.indicators::sizespectrum.db( DS="sizespectrum", p=p )
 


  # -----------------------------
  # lbm; vn="nss_slope"
  for ( vn in p$varstomodel) {
    print(vn)

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="sizespectrum"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   lbm( p=p, tasks=c( "stage0" ) ) # serial mode
    #   lbm( p=p, tasks=c( "continue" ) )    
    lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    lbm( p=p, tasks=c( "save" ) )
    
    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="sizespectrum"  )
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


