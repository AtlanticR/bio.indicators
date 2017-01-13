
  indicators.db = function( ip=NULL, DS="spatial", p=NULL, year=NULL, varname=NULL ) {

    if (DS =="indicators") {

      return( switch( p$project.name,
        sizespectrum = bio.indicators::sizespectrum.db(p=p, DS=p$project.name ),
        metabolism   = bio.indicators::metabolism.db( p=p, DS=p$project.name ), 
        speciesarea  = bio.indicators::speciesarea.db( p=p, DS=p$project.name ),  
        speciescomposition = bio.indicators::speciescomposition.db( p=p, DS=p$project.name ),
        sizespectrum = bio.indicators::sizespectrum.db( p=p, DS=p$project.name ),
        condition =    bio.indicators::condition.db( p=p, DS=p$project.name ),
        biochem =      bio.indicators::biochem.db( p=p, DS=p$project.name ),
        survey =       bio.indicators::survey.db( p=p, DS=p$project.name ),
        NULL
      ) )

    }


    # -----------------------------


    if (DS %in% c("spatial", "spatial.redo") ) {
      # spatial only == static variables
      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain, varname )
      dir.create(outdir, recursive=T, showWarnings=F)

      outfile =  file.path( outdir, "PS.spatial.rdata" )

      if ( DS=="spatial" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = bathymetry.db( p=p, DS="baseline", varnames="all" )  # anything not NULL gives everything with bathymetry.db 
      PS = cbind( PS, substrate.db ( p=p, DS="complete"  ) )
      PS = cbind( PS, temperature.db( p=p, DS="bottom.statistics.climatology" ) )

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    # -----------------------------


    if (DS %in% c("spatial.annual", "spatial.annual.redo") ) {
      # spatial and temporal (annual)
      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain, varname )
      dir.create(outdir, recursive=T, showWarnings=F)

      outfile =  file.path( outdir, paste("PS.spatial.annual", p$prediction.dyear, "rdata", sep=".") )

      if ( DS=="spatial.annual" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      PS = as.list()
      PS[["t"]] = temperature.db( p=p, DS="timeslice" )
      for ( ret in p$bstats ) {
        PS[[ret]] = temperature.db( p=p, DS="bottom.statistics.annual", ret=ret )
      }

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    #---------------------------------


    if (DS %in% c("temperature", "temperature.redo") ) {
      #\\ spatial, temporal (annual and seasonal)
      #\\ copy in array format for domain/resolution of interest for faster lookups
      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain, varname )
      dir.create(outdir, recursive=T, showWarnings=F)

      outfile =  file.path( outdir, "PS.temperature.rdata" )

      if ( DS=="temperature" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }
      nlocs = nrow( bathymetry.db(p=p, DS="baseline"))
      PS = array( NA, dim=c(nlocs, p$ny, p$nw ) )
      for ( y in 1:p$ny ) {
        PS[,y,w] = temperature.db( p=p, DS="predictions", yr=p$yrs[y], ret="mean" )
      }

      save (PS, file=outfile, compress=T )
      return( outfile )
    }

  
    #---------------------------------


    if (DS %in% c("prediction.surface", "prediction.surface.redo") ) {

      outdir = file.path( project.datadirectory("bio.indicators"), "PS", p$spatial.domain, varname )
      dir.create(outdir, recursive=T, showWarnings=F)

      outfile =  file.path( outdir, paste("PS", p$prediction.dyear, "rdata", sep=".") )

      if ( DS=="prediction.surface" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = indicators.db(p=p, DS="spatial")
      nPS = nrow( PS )
      PS = as.list(PS)
      PS = c( PS, indicators.db(p=p, DS="spatial.annual") )
      PS[["t"]] = matrix( indicators.db(p=p, DS="temperature"), nrow=nPS ) # need to make this 2D for lbm .. a limitation of using bigmemory

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    #  -------------------------------


    if (DS %in% c("lbm_inputs", "lbm_inputs.redo") ) {

      INP = bio.indicators::indicators.db( DS="indicators", p=p ) # dependent vars

      # cap quantiles of dependent vars
      dr = list()
      dr[[vm]] = quantile( INP[,vm], probs=p$lbm_quantile_bounds, na.rm=TRUE ) # use 95%CI
      il = which( INP[,vm] < dr[[vm]][1] )
      if ( length(il) > 0 ) INP[il,vm] = dr[[vm]][1]
      iu = which( INP[,vm] > dr[[vm]][2] )
      if ( length(iu) > 0 ) INP[iu,vm] = dr[[vm]][2]

      INP$log.z = log(INP$z)
      INP$log.dZ = log( INP$dZ )
      INP$log.ddZ = log( INP$ddZ)
      INP$log.substrate.grainsize = log(INP$grainsize)
      INP$log.tamplitude = log(INP$tamplitude)
      #   INP$log.tamplitude.climatology = log(INP$tamplitude.climatology)
      INP = INP[, which(names(INP) %in% p$varnames)]  # a data frame


      PS = indicators.db( p=p, DS="prediction.surface" ) # a list object
      PS$log.z = log(PS$z)
      PS$log.dZ = log( PS$dZ )
      PS$log.ddZ = log( PS$ddZ)
      PS$log.substrate.grainsize = log(PS$grainsize)
      PS$log.tamplitude = log(PS$tamplitude)
   #   PS$log.tamplitude.climatology = log(PS$tamplitude.climatology)
      PS = PS[[ which(names(PS) %in% p$varnames) ]] # time vars, if they are part of the model will be created within lbm
      
      OUT = list( LOCS=bathymetry.db(p=p, DS="baseline", COV=PS )          

      return (list(input=INP, output=OUT))

    }


    #  -------------------------------


    if (DS %in% c("complete", "complete.redo") ) {
      # assemble data for a given project 
      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", p$spatial.domain, varname )

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

        for  (vn %in% .. ) {
          
          p$variables$Y = vn # need to send this to get the correct results
          PS[,vn] = cbind( lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="mean")
          
        }

        outfile =  file.path( outdir, paste( "complete", year, "rdata", sep= ".") )

        save (PS, file=outfile, compress=T )
        
      }

      PS = bathymetry.db( p=p, DS="baseline" )
      SS = matrix ...
      for  (vn %in% .. ) {
        
        p$variables$Y = vn
        SS[,vn] = cbind( lbm_db( p=p, DS="stats.to.prediction.grid" )
        colnames(SS) = paste("XXX", colnames(SS), sep=".")
        
      }
      PS = cbind( PS, SS )
      save (PS, file=...)
    

      # spatial warp here to snowcrab grid ...


      return( "Complete" )
    }

    # -------------------

    if (DS %in% c("baseline", "baseline.redo") ) {

      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", p$spatial.domain, varname )

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
