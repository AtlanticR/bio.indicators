
# Generic spatio-temporal interpolations and maping of data
# using the interpolating functions and models defined in bio.indicators / lbm
# NOTE:: could loop this and make automatic but ...
#        because it is so slow right now manually choosing selecting is more reliable (crashes due to power outage, etc ..)
# also, it becomes relatively simple to add other indicators using this approach ..
    
    current.year = 2016


    # project.names = c( "speciescomposition", "speciesarea", "sizespectrum", "metabolism", "condition", "biochem", "..." )
    
    project.names = c( "speciescomposition", "metabolism" )
    
    for ( pn in project.names ){
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


      if (0) {
        # old method
        p$clusters = rep("localhost", 1) # 2015 req 16 GB per run!
        p = make.list( list(vars= p$varstomodel ), Y=p )

        # create model
        parallel.run( habitat.model, DS="redo", p=p )  # in parallel mode  ~ 20 GB + RAM / run
        # habitat.model ( DS="redo", p=p )  # in serial mode

      # 6. predictive interpolation to full domain (iteratively expanding spatial extent)
      #   ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes
        p$clusters = rep("localhost", 10) # 6 GB / process
        p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p )
        parallel.run( habitat.interpolate, p=p, DS="redo" )
        # habitat.interpolate( p=p, DS="redo" )

      # 7. map everything
        p$clusters = rep("localhost", detectCores() )
        p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
        parallel.run( habitat.map, p=p  )
        # habitat.map( p=p  )

        # Finalize: glue all the above together and finalize

        # TODO :: biologicals begin in 1970 ..  need to fix
        #        .. at present data from 1970 are copied to all pre 1970 data years

        p = make.list( list( yrs=p$yearstomodel), Y=p )
        parallel.run(  indicators.db, DS="complete.redo", p=p )
        # indicators.db ( DS="complete.redo", p=p )

      }






