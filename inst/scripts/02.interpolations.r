

# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------
  #required for interpolations and mapping

  p = list( project.name = "speciescomposition" )
  p = indicators.parameters( DS=p$project.name, p=p )

  # p$yearstomodel = 1970:2015  --- change this
   # p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )

  p$clusters = rep("localhost", length( p$varstomodel )  )   ### CAREFUL  .. 2015 req 16 GB per run!
  p = make.list( list(vars= p$varstomodel ), Y=p )
  parallel.run( habitat.model, DS="redo", p=p )
  # habitat.model ( DS="redo", p=p )

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" )
  # habitat.interpolate( p=p, DS="redo" )

  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  )
  # habitat.map( p=p  )


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------



# create base species area stats  ... a few hours
  p = list( project.name = "speciesarea" )
  p = indicators.parameters( DS=p$project.name, p=p )
  # p$yearstomodel = 1970:2015  --- change this
  # p$varstomodel = c( "C", "Z", "T", "Npred" )

  # choose:
  # n  = 2
  # n = detectCores()  # this is the default
  # p$clusters = rep( "localhost", n )
  # p$clusters = rep(c("kaos", "nyx", "tartarus"), n)

  p$clusters = rep("localhost", length( p$varstomodel ) )
  p = make.list( list(vars= p$varstomodel ), Y=p )
  parallel.run( habitat.model, DS="redo", p=p )
  # habitat.model ( DS="redo", p=p )

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" )
  #  habitat.interpolate( p=p, DS="redo" )

  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  )
  # habitat.map( p=p  )


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
  # analysis and spatial database of normalised size spectrum, average size

  p = list( project.name = "sizespectrum" )
  p = indicators.parameters( DS=p$project.name, p=p )
  # p$yearstomodel = 1970:2015  --- change this
  # p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep(c("kaos", "nyx", "tartarus"), 2)
  # p$clusters = rep("localhost", detectCores() )

  p$clusters = rep("localhost", length( p$varstomodel ) )
  p = make.list( list(vars= p$varstomodel ), Y=p )
  parallel.run( habitat.model, DS="redo", p=p )
  # habitat.model ( DS="redo", p=p )

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" )
  # habitat.interpolate( p=p, DS="redo" )


  # map everything
  # p$clusters = rep( "localhost", 8 )
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p )
  # habitat.map( p=p )



#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------


  p = list( project.name = "metabolism" )
  p = indicators.parameters( DS=p$project.name, p=p )

  # p$yearstomodel = 1970:2015  --- change this
  # p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )

  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
   #p$clusters = rep("localhost", detectCores() )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("localhost", 24 ) )

  ## WARNING takes 16GB / run!!
  p$clusters = rep("localhost", length( p$varstomodel ) )
  p = make.list( list(vars= p$varstomodel ), Y=p )  #
  parallel.run( habitat.model, DS="redo", p=p )
  # habitat.model ( DS="redo", p=p )

  # p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list( yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" )


  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  )
  # habitat.map( p=p  )




#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------



  p = list( project.name = "condition" )
  p = indicators.parameters( DS=p$project.name, p=p )
  # p$yearstomodel = 1970:2015  --- change this
  # p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
  #                     "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )

  # choose:
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep("tartarus", 24), rep("kaos", 17 ) )
  # p$clusters = rep("localhost", detectCores() )

  # create a spatial interpolation model for each variable of interest
  # full model requires 5 GB per model
  # ~ 30 hrs with 2 CPUs @ 3.6 Ghz
  # 200 hr! in 2015
  p$clusters = rep("localhost", length( p$varstomodel ) )
  p = make.list( list(vars= p$varstomodel ), Y=p )
  parallel.run( habitat.model, DS="redo", p=p )
  # habitat.model ( DS="redo", p=p )

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" )
  # habitat.interpolate( p=p, DS="redo" )

  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  )
  # habitat.map( p=p  )



#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# glue all the above together and finalize

  # TODO :: biologicals begin in 1970 ..  need to fix
  #        .. at present data from 1970 are copied to all pre 1970 data years

  p = make.list( list( yrs=p$yearstomodel), Y=p )
  parallel.run(  habitat.db, DS="complete.redo", p=p )
  # habitat.db ( DS="complete.redo", p=p )

