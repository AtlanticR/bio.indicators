
  indicators.db = function( ip=NULL, DS="baseline", p=NULL, year=NULL, voi=NULL, varnames=NULL ) {

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

      INP = INP[, which(names(INP) %in% c(p$varnames, p$variables$Y, p$variables$TIME, "dyear", "yr" ) ) ]  # a data frame
      oo = setdiff(p$varnames, names(INP))
      if (length(oo) > 0 ) {
        print(oo )
        warning("Some variables are missing in the input data")
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

      ps_varnames = setdiff( p$varnames, p$variables$LOCS )

      PS = PS[ which(names(PS) %in% ps_varnames ) ] # time vars, if they are part of the model will be created within lbm

      oo = setdiff(p$varnames, ps_varnames )
      if (length(oo) > 0 ) {
        print(oo )
        warning("Some variables are missing in the prediction surface, PS")
      }

      OUT = list( LOCS=bathymetry.db(p=p, DS="baseline"), COV=PS )    

      return (list(input=INP, output=OUT))

    }


    # ------------------


    if ( DS %in% c("predictions", "predictions.redo" ) ) {
      # NOTE: the primary interpolated data were already created by lbm. 
      # This routine points to this data and also creates 
      # subsets of the data where required, determined by "spatial.domain.subareas" 
   
      outdir = file.path(project.datadirectory("bio.temperature"), "modelled", voi, p$spatial.domain )
      
      if (DS %in% c("predictions")) {
        P = V = NULL
        fn = file.path( outdir, paste("lbm.prediction", ret,  yr, "rdata", sep=".") )
        if (is.null(ret)) ret="mean"
        if (file.exists(fn) ) load(fn) 
        if (ret=="mean") return (P)
        if (ret=="sd") return( V)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if ( is.null(ip) ) ip = 1:p$nruns

      # downscale and warp from p(0) -> p1

      for ( r in ip ) {
        # print (r)
        yr = p$runs[r, "yrs"]
        # default domain
        PP0 = lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="mean")
        VV0 = lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="sd")
        p0 = spatial_parameters( p=p, type=p$spatial.domain ) # from
        L0 = bathymetry.db( p=p0, DS="baseline" )
        L0i = lbm::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
        sreg = setdiff( p$spatial.domain.subareas, p$spatial.domain ) 

        for ( gr in sreg ) {
          p1 = spatial_parameters( p=p, type=gr ) # 'warping' from p -> p1
          L1 = bathymetry.db( p=p1, DS="baseline" )
          L1i = lbm::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
          L1 = planar2lonlat( L1, proj.type=p1$internal.crs )
          L1$plon_1 = L1$plon # store original coords
          L1$plat_1 = L1$plat
          L1 = lonlat2planar( L1, proj.type=p0$internal.crs )
          p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres, xwidth=4*p1$pres, ywidth=4*p1$pres )
          P = spatial_warp( PP0[], L0, L1, p0, p1, "fast", L0i, L1i )
          V = spatial_warp( VV0[], L0, L1, p0, p1, "fast", L0i, L1i )
          outdir_p1 = file.path(project.datadirectory("bio.indicators"), "modelled", voi, p1$spatial.domain)
          dir.create( outdir_p1, recursive=T, showWarnings=F )
          fn1_sg = file.path( outdir_p1, paste("lbm.prediction.mean",  yr, "rdata", sep=".") )
          fn2_sg = file.path( outdir_p1, paste("lbm.prediction.sd",  yr, "rdata", sep=".") )
          save( P, file=fn1_sg, compress=T )
          save( V, file=fn2_sg, compress=T )
          print (fn1_sg)
        }
      } 
      return ("Completed")

      if (0) {
        aoi = which( PS$z > 5 & PS$z < 3000 & PS$z.range < 500)
        levelplot( log(z) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
        levelplot( log(t.ar_1) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
        
        levelplot( log(t.range) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
        levelplot( Z.rangeSD ~ plon + plat, PS[aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      }
    
    }



    #  -------------------------------

    if (DS %in% c(  "lbm.stats", "lbm.stats.redo" )){

      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", voi, p$spatial.domain )
      
      if (DS %in% c("lbm.stats")) {
        stats = NULL
        fn = file.path( outdir, paste( "lbm.statistics", "rdata", sep=".") )
        if (file.exists(fn) ) load(fn) 
        return( stats )
      }

      # downscale and warp from p(0) -> p1
      # default domain
      S0 = lbm_db( p=p, DS="stats.to.prediction.grid" )
      Snames = colnames(S0)
      p0 = spatial_parameters( p=p, type=p$spatial.domain ) # from
      L0 = bathymetry.db( p=p0, DS="baseline" )
      L0i = lbm::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
      sreg = setdiff( p$spatial.domain.subareas, p$spatial.domain ) 

      for ( gr in sreg ) {
        p1 = spatial_parameters( p=p, type=gr ) # 'warping' from p -> p1
        L1 = bathymetry.db( p=p1, DS="baseline" )
        L1i = lbm::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
        L1 = planar2lonlat( L1, proj.type=p1$internal.crs )
        L1$plon_1 = L1$plon # store original coords
        L1$plat_1 = L1$plat
        L1 = lonlat2planar( L1, proj.type=p0$internal.crs )
        p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres, xwidth=4*p1$pres, ywidth=4*p1$pres )
        stats = matrix( NA, ncol=ncol(S0), nrow=nrow(L1) )
        for ( i in 1:ncol(S0) ) {
          stats[,i] = spatial_warp( S0[,i], L0, L1, p0, p1, "fast", L0i, L1i )
        }
        colnames(stats) = Snames
        outdir_p1 = file.path(project.datadirectory("bio.indicators"), "modelled", voi, p1$spatial.domain)
        dir.create( outdir_p1, recursive=T, showWarnings=F )
        fn1_sg = file.path( outdir_p1, paste("lbm.statistics", "rdata", sep=".") )
        save( stats, file=fn1_sg, compress=T )
        print (fn1_sg)
      }
      return ("Completed")
    }


    #  -------------------------------


    if (DS %in% c("complete", "complete.redo") ) {
      # assemble data for a given project 
      outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", voi, p$spatial.domain )

      dir.create(outdir, recursive=T, showWarnings=F)

      if (DS=="complete") {
        IC = NULL
        outdir =  file.path( project.datadirectory("bio.indicators"), "modelled", voi, p$spatial.domain )
        outfile =  file.path( outdir, paste( "indicators", "complete", p$spatial.domain, "rdata", sep= ".") )
        if ( file.exists( outfile ) ) load( outfile )
        Inames = names(IC)
        if (is.null(varnames)) varnames=Inames
        varnames = intersect( Inames, varnames )
        if (length(varnames) == 0) varnames=Inames  # no match .. send all
        IC = IC[ , varnames]
        return(IC)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if (is.null(ip)) ip = 1:p$nruns

      grids = unique( c(p$spatial.domain.subareas , p$spatial.domain ) ) # operate upon every domain
   
      for (gr in grids ) {
        print(gr)

        p1 = spatial_parameters( type=gr ) #target projection    
        L1 = bathymetry.db(p=p1, DS="baseline")

        BS = indicators.db( p=p1, DS="lbm.stats" )
        colnames(BS) = paste("t", colnames(BS), sep=".")
        IC = cbind( L1, BS )

        # climatology
        nL1 = nrow(L1)
        PS = PSsd = matrix( NA, nrow=nL1, ncol=p$ny )
        p$variables$Y = voi # need to send this to get the correct results
        for (iy in ip) {
          yr = p$runs[iy, "yrs"]
          PS[,vn] = lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="mean")
          PSsd[,vn] = lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="sd")
        }

        CL = cbind( apply( PS, 1, mean, na.rm=TRUE ), apply( PSsd, 1, mean, na.rm=TRUE ) )
        colnames(CL) = paste( voi, c("mean", "sd"), "climatology", sep=".")
        IC = cbind( IC, CL )
        PS = PSsd = NULL

        outdir = file.path(project.datadirectory("bio.indicators"), "modelled", voi, p1$spatial.domain)
        dir.create( outdir, recursive=T, showWarnings=F )
        outfile =  file.path( outdir, paste( "indicators", "complete", p1$spatial.domain, "rdata", sep= ".") )
        save( IC, file=outfile, compress=T )
        print( outfile )

      }
      return( "Complete" )
    }

    # -------------------

    if (DS %in% c("baseline", "baseline.redo") ) {


      if ( DS=="baseline" ) {
        BL = list()
        for (voi in varnames ) {
          outdir = file.path(project.datadirectory("bio.indicators"), "modelled", voi, p$spatial.domain)
          outfile =  file.path( outdir, paste( "indicators", "baseline", p1$spatial.domain, "rdata", sep= ".") )
          TS = NULL
          load( outfile)
          BL[[voi]] = TS
        }
        return (BL)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if (is.null(ip)) ip = 1:p$nruns

      grids = unique( c(p$spatial.domain.subareas , p$spatial.domain ) ) # operate upon every domain
   
      for (gr in grids ) {
        print(gr)
        p1 = spatial_parameters( type=gr ) #target projection    
        L1 = bathymetry.db(p=p1, DS="baseline")
        nL1 = nrow(L1)
        TS = matrix( NA, nrow=nL1, ncol=p$ny )
        p$variables$Y = voi # need to send this to get the correct results
        for (i in 1:p$ny ) {
          yr = p$yrs[i]
          TS[,i] = lbm_db( p=p, DS="lbm.prediction", yr=yr, ret="mean")
         }

        outdir = file.path(project.datadirectory("bio.indicators"), "modelled", voi, p1$spatial.domain)
        dir.create( outdir, recursive=T, showWarnings=F )
        outfile =  file.path( outdir, paste( "indicators", "baseline", p1$spatial.domain, "rdata", sep= ".") )
        save( TS, file=outfile, compress=T )
        print( outfile )
      }

      return( "Complete" )
 
    }

 }
