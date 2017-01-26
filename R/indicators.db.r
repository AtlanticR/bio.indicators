
  indicators.db = function( ip=NULL, DS="baseline", p=NULL, year=NULL, voi=NULL ) {

    # over-ride default dependent variable name if it exists
    if (exists("variables",p)) if(exists("Y", p$variables)) voi=p$variables$Y

    if (DS =="indicators") {

      out = switch( p$project.name,
        sizespectrum = bio.indicators::sizespectrum.db(p=p, DS=p$project.name ),
        metabolism   = bio.indicators::metabolism.db( p=p, DS=p$project.name ), 
        speciesarea  = bio.indicators::speciesarea.db( p=p, DS=p$project.name ),  
        speciescomposition = bio.indicators::speciescomposition.db( p=p, DS=p$project.name ),
        sizespectrum = bio.indicators::sizespectrum.db( p=p, DS=p$project.name ),
        condition =    bio.indicators::condition.db( p=p, DS=p$project.name ),
        biochem =      bio.indicators::biochem.db( p=p, DS=p$project.name ),
        survey =       bio.indicators::survey.db( p=p, DS=p$project.name ),
        NULL
      ) 

      return(out)

    }


    # -----------------------------


    if (DS %in% c("spatial", "spatial.redo") ) {
      # spatial only == static variables
      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain )
      dir.create(outdir, recursive=T, showWarnings=F)

      outfile =  file.path( outdir, "PS.spatial.rdata" )

      if ( DS=="spatial" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = bathymetry.db( p=p, DS="baseline", varnames="all" )  # anything not NULL gives everything with bathymetry.db 
      p0 = bio.temperature::temperature.parameters(p=p, current.year=p$current.year )
      p0 = bio.temperature::temperature.parameters( DS="lbm", p=p0 )
      p0 = bio.spacetime::spatial_parameters( p=p0, type=p$spatial.domain ) # return to correct domain      
      PS = cbind( PS, substrate.db ( p=p, DS="complete"  ) )
      
      # override voi (variable of interest) to obtain results
      p0 = bio.temperature::temperature.parameters(p=p, current.year=p$current.year )
      p0 = bio.temperature::temperature.parameters( DS="lbm", p=p0 )
      p0 = bio.spacetime::spatial_parameters( p=p0, type=p$spatial.domain ) # return to correct domain      
      tclim = temperature.db( p=p0, DS="bottom.statistics.climatology" ) 

      names(tclim) = paste(names(tclim), "climatology", sep="." )
      PS = cbind( PS, tclim)

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    # -----------------------------


    if (DS %in% c("spatial.annual", "spatial.annual.redo") ) {
      # spatial and temporal (annual)
      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain )
      dir.create(outdir, recursive=T, showWarnings=F)

      dyear_index = 1
      if (exists("dyears", p) & exists("prediction.dyear", p))  dyear_index = which.min( abs( p$prediction.dyear - p$dyears))

      outfile =  file.path( outdir, paste("PS.spatial.annual", dyear_index, "rdata", sep=".") )

      if ( DS=="spatial.annual" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      p0 = bio.temperature::temperature.parameters(p=p, current.year=p$current.year )
      p0 = bio.temperature::temperature.parameters( DS="lbm", p=p0 )
      p0 = bio.spacetime::spatial_parameters( p=p0, type=p$spatial.domain ) # return to correct domain      
 
      PS = list()
      PS[["t"]] = temperature.db( p=p0, DS="timeslice", ret="mean" )
      for ( ret in p0$bstats ) {
        PS[[ret]] = temperature.db( p=p0, DS="bottom.statistics.annual", ret=ret )
      }

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    #---------------------------------


    if (DS %in% c("spatial.annual.seasonal", "spatial.annual.seasonal.redo") ) {
      #\\ spatial, temporal (annual and seasonal)
      # at present only temperatute varies at this scale
      #\\ copy in array format for domain/resolution of interest for faster lookups
      message ("not used right now as predicting at every time-space-dyear is very expensive")
      if ( DS=="spatial.annual.seasonal.redo" ){
        message( "At present only temperature varies at this scale .. nothing else needs to be done." )
        return(NULL)
      }
      out = temperature.db(p=p, DS="spatial.annual.seasonal" )
      return( out )
    }

  
    #---------------------------------


    if (DS %in% c("prediction.surface", "prediction.surface.redo") ) {

      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain )
      dir.create(outdir, recursive=T, showWarnings=F)
    
      dyear_index = 1
      if (exists("dyears", p) & exists("prediction.dyear", p))  dyear_index = which.min( abs( p$prediction.dyear - p$dyears))

      outfile =  file.path( outdir, paste("PS", dyear_index, "rdata", sep=".") )

      if ( DS=="prediction.surface" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = indicators.db(p=p, DS="spatial")
      names(PS)[which(names(PS)=="tmean")] = "tmean.climatology"
      names(PS)[which(names(PS)=="tsd")] = "tsd.climatology"
      names(PS)[which(names(PS)=="tmin")] = "tmin.climatology"
      names(PS)[which(names(PS)=="tmax")] = "tmax.climatology"
      names(PS)[which(names(PS)=="amplitude")] = "tamplitude.climatology"
      
      nPS = nrow( PS )
      PS = as.list(PS)
      
      p0 = bio.temperature::temperature.parameters(p=p, current.year=p$current.year )
      p0 = bio.temperature::temperature.parameters( DS="lbm", p=p0 )
      p0 = bio.spacetime::spatial_parameters( p=p0, type=p$spatial.domain ) # return to correct domain      
 
      yr_index = match( p$yrs, p0$tyears ) 
      u = indicators.db(p=p, DS="spatial.annual")
      for ( vn in names(u) ){
        u[[vn]] = u[[vn]][,yr_index] 
      }
      PS = c( PS, u)
      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    #  -------------------------------


    if (DS %in% c("lbm_inputs", "lbm_inputs.redo") ) {

      INP = bio.indicators::indicators.db( DS="indicators", p=p ) # dependent vars
      INP$tiyr = lubridate::decimal_date( INP$timestamp ) 

      locsmap = match( 
        lbm::array_map( "xy->1", INP[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams ) )

      # spatial vars and climatologies 
      newvars = c("dZ", "ddZ", "log.substrate.grainsize", "tmean", "tsd" )
      sn = indicators.lookup( p=p, DS="spatial", locsmap=locsmap, varnames=newvars )
      names( sn ) = c("dZ", "ddZ", "log.substrate.grainsize", "tmean.climatology", "tsd.climatology" )
      INP = cbind( INP,  sn )

      # for space-time(year-averages) 
      newvars = c( "tmean", "tsd", "amplitude" )
      sn = indicators.lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=INP[,"timestamp"], varnames=newvars )
      colnames( sn  ) = newvars
      INP = cbind( INP,  sn )
      INP$tamplitude = INP$amplitude

      INP = INP[, which(names(INP) %in% p$varnames)]  # a data frame
      oo = setdiff(p$varnames, names(INP))
      if (length(oo) > 0 ) {
        message("Some variables are missing in the input data")
        print(oo )
        stop()
      }
      INP = na.omit(INP)

    # cap quantiles of dependent vars
      dr = list()
      for (voi in p$varnames) {
        dr[[voi]] = quantile( INP[,voi], probs=p$lbm_quantile_bounds, na.rm=TRUE ) # use 95%CI
        il = which( INP[,voi] < dr[[voi]][1] )
        if ( length(il) > 0 ) INP[il,voi] = dr[[voi]][1]
        iu = which( INP[,voi] > dr[[voi]][2] )
        if ( length(iu) > 0 ) INP[iu,voi] = dr[[voi]][2]
      }

      PS = indicators.db( p=p, DS="prediction.surface" ) # a list object with static and annually varying variables  
      names(PS)[ names(PS)=="amplitude"] ="tamplitude" 

      PS = PS[ which(names(PS) %in% p$varnames) ] # time vars, if they are part of the model will be created within lbm

      oo = setdiff(p$varnames, names(PS))
      if (length(oo) > 0 ) {
        message("Some variables are missing in the prediction surface, PS")
        print(oo )
        stop()
      }


      OUT = list( LOCS=bathymetry.db(p=p, DS="baseline", COV=PS ) )         

      return (list(input=INP, output=OUT))

    }


    #  -------------------------------


    if (DS %in% c("complete", "complete.redo") ) {
      # assemble data for a given project 
      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", voi, p$spatial.domain )

      dir.create(outdir, recursive=T, showWarnings=F)

      if ( DS=="complete" ) {
        outfile =  file.path( outdir, paste( "complete", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        return (PS)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if (is.null(ip)) ip = 1:p$nruns


      for (iy in ip) {
        yr = p$runs[iy, "yrs"]
        
        PS = matrix( ... )

        for  (vn in ... ) {
          
          p$variables$Y = vn # need to send this to get the correct results
          PS[,vn] = cbind( lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="mean") )
          
        }

        outfile =  file.path( outdir, paste( "complete", year, "rdata", sep= ".") )

        save (PS, file=outfile, compress=T )
        
      }

      PS = bathymetry.db( p=p, DS="baseline" )
#      SS = matrix ...
      for  (vn in ... ) {
        
        p$variables$Y = vn
        SS[,vn] = cbind( lbm_db( p=p, DS="stats.to.prediction.grid" ) )
        colnames(SS) = paste("XXX", colnames(SS), sep=".")
        
      }
      PS = cbind( PS, SS )
 #     save (PS, file=...)
    

      # spatial warp here to snowcrab grid ...

      indicators.db(p=p, DS="baseline.add")  # add the "complete" data tables to the current version of "baseline"

      return( "Complete" )
    }

    # -------------------

    if (DS %in% c("baseline", "baseline.add") ) {

      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", p$spatial.domain )

      dir.create(outdir, recursive=T, showWarnings=F)

      if ( DS=="baseline" ) {
        outfile =  file.path( outdir, paste( "baseline", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        return (PS)
      }

      # pick and choose and glue all "complete" project data


      # spatial warp here to snowcrab grid ...

    }

 }
