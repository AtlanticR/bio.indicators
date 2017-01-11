
# Generic spatio-temporal interpolations and maping of data
# using the interpolating functions and models defined in bio.indicators / lbm
# NOTE:: could loop this and make automatic but ...
#        because it is so slow right now manually choosing selecting is more reliable (crashes due to power outage, etc ..)
# also, it becomes relatively simple to add other indicators using this approach ..
    
    current.year = 2016
 
    ## NOTE resolution is fixed at SSE

    # project.names = c( "speciescomposition", "speciesarea", "sizespectrum", "metabolism", "condition", "biochem", "..." )
    
    project.names = c( "speciescomposition", "metabolism" )
    
    for ( pn in project.names ) {

      p = NULL
      p = bio.indicators::indicators.parameters( DS=project.names, current.year=current.year )
      p = bio.indicators::indicators.parameters( p=p, DS="lbm" )
      DATA = 'indicators.db( p=p, DS="lbm_inputs" )'
      
      for ( v in p$varstomodel) {
        p = lbm( p=p, DATA=DATA, v=v ) # the interpolation
        p = make.list( list( yrs=p$yearstomodel), Y=p ) 
        parallel.run( indicators.db, p=p, DS="lbm.prediction.redo", v=v ) # reformat the interpolations/predictions
        indicators.db( p=p, DS="lbm.finalize.redo", v=v ) # assimilate into database 
        p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
        parallel.run( habitat.map, p=p  ) # a few figures
      }

    }

    # glue everything together
    p = make.list( list( yrs=p$yearstomodel), Y=p )
    parallel.run(  indicators.db, DS="complete.redo", p=p )
    # indicators.db ( DS="complete.redo", p=p )


    p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
    parallel.run( habitat.map, p=p  )
    # habitat.map( p=p  )

    # TODO :: biologicals begin in 1970 ..  need to fix
    #        .. at present data from 1970 are copied to all pre 1970 data years

  







