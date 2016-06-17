
# Generic spatio-temporal interpolations and maping of data
# using the interpolating functions and models defined in bio.indicators / bio.habitat
# NOTE:: could loop this and make automatic but ...
#        because it is so slow right now manually choosing selecting is more reliable (crashes due to power outage, etc ..)
# also, it becomes relatively simple to add other indicators using this approach ..

  # 1. choose a parameter list
    p = bio.indicators::indicators.parameters( DS="speciescomposition" )
    p = bio.indicators::indicators.parameters( DS="speciesarea" )
    p = bio.indicators::indicators.parameters( DS="sizespectrum" )
    p = bio.indicators::indicators.parameters( DS="metabolism" )
    p = bio.indicators::indicators.parameters( DS="condition" )


  # 2. choose vars if different from default
    # p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )  # species composition
    # p$varstomodel = c( "C", "Z", "T", "Npred" ) # speciesarea
    # p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )  # sizespectrum
    # p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )  # metabolism
    # p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
    #                     "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )


  # 3. choose years and vars if different from default
    # p$yearstomodel = 2000:lubridate::year(lubridate::now())
    p$yearstomodel = 2010:2015


  # 4. choose clusters
    # n  = 2
    # n = detectCores()  # this is the default
    # p$clusters = rep( "localhost", n )
    # p$clusters = rep(c("kaos", "nyx", "tartarus"), n)
    p$clusters = rep("localhost", length( p$varstomodel )  )   ### CAREFUL  .. 2015 req 16 GB per run!
    p = make.list( list(vars= p$varstomodel ), Y=p )


  # 5. create model
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



#---------------------------------------------------------------------------
# Finalize: glue all the above together and finalize

  # TODO :: biologicals begin in 1970 ..  need to fix
  #        .. at present data from 1970 are copied to all pre 1970 data years

  p = make.list( list( yrs=p$yearstomodel), Y=p )
  parallel.run(  indicators.db, DS="complete.redo", p=p )
  # indicators.db ( DS="complete.redo", p=p )

