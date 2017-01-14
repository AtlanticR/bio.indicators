
  speciesarea.db = function( DS="", p=NULL, yr=NULL ) {

    outdir = file.path( project.datadirectory("bio.indicators"), "speciesarea" )

    dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
    infix = paste( p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse="."), p$speciesarea.method, sep="." )


    if (DS %in% c("speciesarea.counts", "speciesarea.counts.ny", "speciesarea.counts.redo") ) {

      fn = file.path( outdir, paste( "speciesarea.counts", infix, "rdata", sep=".") )
      fn.ny = file.path( outdir, paste( "speciesarea.counts.ny", infix, "rdata", sep=".") )

      if (DS=="speciesarea.counts") {
        load( fn)
        return (SA)
      }
      if (DS=="speciesarea.counts.ny") {
        load( fn.ny)
        return (SA.ny)
      }

      set = survey.db (DS="set", p=p)
      scat = survey.db (DS="cat", p=p)

      p$nsets = nrow( set )
      p$nlengthscale = length(p$lengthscale)
      p$ntimescale = length(p$timescale)

      if (p$use.bigmemory.file.backing) {

        p$fn.tmp = file.path(  make.random.string("speciesarea.bigmemory.tmp" ))
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        p$fn.ny.tmp = file.path(  make.random.string("speciesarea.ny.bigmemory.tmp" ) )
        p$fn.ny.desc = paste( p$fn.ny.tmp, "desc", sep=".")

        sar = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale,
            type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc  )

        sar.ny = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale,
            type="double" , init=NA, backingfile=p$fn.ny.tmp, descriptorfile=p$fn.ny.desc )

      } else {

        sar = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, type="double" , init=NA  )
        sar.ny = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, type="double", init=NA )

      }


      p$bigmem.desc = bigmemory::describe(sar)
      p$bigmem.ny.desc =  bigmemory::describe(sar.ny)  # counts the # of years of data

      p = make.list( list( nsets=1:p$nsets ), Y=p )
      parallel.run( species.count.engine, p=p, set=set, sc=scat )

      sar <- attach.big.matrix( p$bigmem.desc )
      sar.ny <- attach.big.matrix( p$bigmem.ny.desc )

      SA = array( data=sar[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )
      SA.ny = array( data=sar.ny[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )

      save( SA, file=fn, compress=T )
      save( SA.ny, file=fn.ny, compress=T )

      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
        file.remove( p$fn.ny.tmp )
        file.remove( p$fn.ny.desc )
      }

      return( fn )
    }


    if (DS %in% c("speciesarea.stats","speciesarea.stats.redo") ) {

      fn = file.path( outdir, paste("speciesarea.stats", infix, "rdata", sep=".") )

      if (DS=="speciesarea.stats") {
        load( fn)
        return ( sa )
      }

      SA = speciesarea.db( DS="speciesarea.counts", p=p )

      p$nvars = 9

      p$nsets = nrow(SA)
      rm(SA); gc()

      if (p$use.bigmemory.file.backing) {
        p$fn.tmp = file.path( make.random.string("speciesarea.stats.bigmemory.tmp") )
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        o = big.matrix(nrow=p$nsets, ncol=p$nvars,
            type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc)
      } else {
        o = big.matrix(nrow=p$nsets, ncol=p$nvars, type="double", init=NA )
      }

      p$bigmem.desc = bigmemory::describe(o)

      p = make.list( list( nsets=1:p$nsets ), Y=p )
      parallel.run( speciesarea.statistics, p=p )

      o <- attach.big.matrix( p$bigmem.desc )
      o = as.data.frame(o[])

      names( o ) = c( "C", "Z", "T", "C.se", "Z.se", "T.se", "sar.rsq", "Npred", "Npred.se"   )
      o = factor2number( o, c( "C", "Z", "T", "C.se", "Z.se", "T.se", "sar.rsq", "Npred", "Npred.se"   ) )

      # save ( o, file=fn, compress=T )

      set = survey.db (DS="set", p=p)

      if ( nrow(set) != nrow(o ) ) {
        print( "Error: data merge failure" )
        stop( nrow(o) )
      }

      sa = cbind( set, o )
      save ( sa, file=fn, compress=T )

      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
      }

      return( fn )

    }


    # --------------------


    if (DS %in% c( "speciesarea", "speciesarea.redo" ) ) {

      fn = file.path( outdir, paste( "set.speciesarea.merged", infix, "rdata", sep="." ) )

      if (DS=="speciesarea") {
        SA = NULL
        if (file.exists( fn) ) load( fn )
        return ( SA )
      }

      SA = speciesarea.db( DS="speciesarea.stats", p=p )
      SA = lonlat2planar( SA, proj.type=p$internal.projection )

      # this forces resolution of p$pres=1km in SSE
      SA$lon = SA$lat = NULL

      yrs = sort( unique( SA$yr ) )

      # check for duplicates
      for ( y in p$yearstomodel ) {
        yy = which (SA$yr == y)
        ii = which( duplicated( SA$id[yy] ) )

        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( SA[yy,] [ duplicates.toremove( SA$id[yy] ) ] )
          SA = SA[ - ii,]
        }
      }
   
      locsmap = match( 
        lbm::array_map( "xy->1", SA[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams ) )

      SA = cbind( SA, indicators.lookup( p=p, DS="spatial", locsmap=locsmap ) )
      SA = cbind( SA, indicators.lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=SA[,"timestamp"] ))
      SA$t = indicators.lookup( p=p, DS="temperature",   locsmap=locsmap, timestamp=SA[,"timestamp"] )

      oo = which(!is.finite( SA$plon+SA$plat ) )
      if (length(oo)>0) SA = SA[ -oo , ]  # a required field for spatial interpolation

      save( SA, file=fn, compress=T )
      return (fn)
    }

  }


