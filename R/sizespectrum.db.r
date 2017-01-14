
  sizespectrum.db = function( DS="", p=NULL ) {
    ### dependency is only groundfish db for now. ...

    outdir = file.path( project.datadirectory("bio.indicators"), "sizespectrum" )

    dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
    infix = paste( p$nss.taxa, p$nss.type, p$nss.base, sep="." )

    if (DS %in% c("sizespectrum.by.set", "sizespectrum.by.set.redo") ) {

      # make the base normalised size spectral statistics summaries

      if (DS == "sizespectrum.by.set" ) {
        fn = file.path( outdir, paste(  "sizespectrum.by.set", infix, "rdata", sep="." ) )
        load( fn )
        return (ss )
      }

      x =  survey.db( "det" )  # mass and length are not transformed
      x = x[ which( x$data.source=="groundfish"), ]

      # x = x[ which(x$settype %in% c(1,2,5) ), ]
      # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
      #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
      #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

       for (tx in p$nss.taxa) {

        #i.tx = taxonomy.filter.taxa( x$spec_bio, taxafilter=tx)
       i.tx = taxonomy.filter.taxa( x$spec_bio, method=p$taxa, outtype="internalcodes" )
        if ( is.null(i.tx) || length(i.tx) < 30) next()
        XX = x[ i.tx, ]
        rm( i.tx ); gc()

        for (vname in p$nss.type) {

          XX.log = log( XX[,vname], base=p$nss.base )
          XX$sizeclass = cut( XX.log, breaks=p$nss.bins$lb, labels=F, include.lowest=F, right=T )

          jjj = which( is.finite(XX$sizeclass + XX$cfdet) )
          XX = XX[jjj,]

          XX$id=as.factor(XX$id)
          XX$sizeclass=as.factor(XX$sizeclass)

          # closed on the right: (x,x]
          # midpoints = (l.bound [2:n.size] + l.bound [1:(n.size-1)] ) /2
         infix = paste( tx, vname, p$nss.base, sep="." )

          fn = file.path( outdir, paste(  "sizespectrum.by.set", infix, "rdata", sep="." ) )

          ss = NULL
          tt = XX$cfdet*XX[,vname]
          ss = xtab.2way( xval=tt, factors=XX[,c("id", "sizeclass")] )

          ### ss contains number per km2 broken down by age class and weighted for sa, etc
          save( ss, file=fn, compress=T)

          rm (XX, ss); gc()
        }
      }
      return( "Done" )
    }


    # --------------------


    if (DS %in% c( "sizespectrum.stats", "sizespectrum.stats.redo" ) ) {


      fn = file.path( outdir, paste( "set.sizespectrum.stats", infix, "rdata", sep=".")  )

      if (DS=="sizespectrum.stats") {
        nss = NULL
        if (file.exists( fn) ) load( fn )
        return ( nss )
      }

      sm = survey.db( "set" )
      sm = sm[ which( sm$data.source=="groundfish") ,]
      sm$area = sm$sa
      sm$sa = NULL

      sm$id = as.character( sm$id )
      smdups = which( duplicated (sm$id ) )
      if (length(smdups) > 0) sm = sm[ -smdups, ]
      sm = sm[ order(sm$id) , ]
      gc()

      nss = NULL
      p$newvars = c( "id",  "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax" )

      p$nsets = nrow( sm )
      p$nlengthscale = length(p$nss.bins)
      p$ntimescale = length(p$nss.stimes)

      if (p$use.bigmemory.file.backing) {
        p$fn.tmp = file.path(  make.random.string( "nss.bigmemory.tmp" ) )
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        nss = big.matrix( nrow=p$nsets, ncol=length(p$newvars), type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc )
      } else {
        nss = big.matrix( nrow=p$nsets, ncol=length(p$newvars), type="double" , init=NA, shared=TRUE )
      }

      p$bigmem.desc = bigmemory::describe(nss)

      p = make.list( list( nsets=1:p$nsets ), Y=p )
      parallel.run( sizespectrum.compute, p=p, sm=sm )
      # sizespectrum.compute( p=p, sm=sm )

      nss <- attach.big.matrix( p$bigmem.desc )
      nss = as.data.frame(as.matrix(nss) )
      names(nss) = p$newvars

      nss = factor2number( nss, c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1","nss.shannon", "nss.evenness", "nss.Hmax" ) )
      nss = factor2character( nss, c("id") )

      nss$id = sort( unique(as.character( sm$id ) ) )  # overwrite

      save(nss, file=fn, compress=T)

      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
      }

      return ( "Done" )
    }


    # --------------------


    if (DS %in% c("sizespectrum", "sizespectrum.redo") ) {

      # make the base normalised size spectral statistics summaries

      fn = file.path( outdir, paste( "sizespectrum", infix, "rdata", sep="." ) )

      if ( DS=="sizespectrum" ) {
        SS = NULL
        if (file.exists( fn) ) load( fn )
        return ( SS )
      }

      sm = sizespectrum.db( DS="sizespectrum.stats", p=p )
      smg = groundfish.db( "set.base" )
      smg$dyear = lubridate::decimal_date( smg$timestamp ) - smg$yr
      smg$z = smg$sdepth
      smg$t = smg$temp
      smg$sdepth = NULL
      smg$temp = NULL

      sm = merge (sm, smg, by="id", all.x=T, all.y=F, sort= F)
      sm = lonlat2planar( sm, proj.type=p$internal.projection )

      # check for duplicates
      for ( y in p$yearstomodel ) {
        yy = which (sm$yr == y)
        ii = which( duplicated( sm$id[yy] ) )

        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( sm[yy,] [ duplicates.toremove( sm$id[yy] ) ] )
          sm = sm[ - ii,]
        }
      }

      locsmap = match( 
        lbm::array_map( "xy->1", sm[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams ) )

      sm = cbind( sm, indicators.lookup( p=p, DS="spatial", locsmap=locsmap ) )
      sm = cbind( sm, indicators.lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=sm[,"timestamp"] ))
      sm$t = indicators.lookup( p=p, DS="temperature",   locsmap=locsmap, timestamp=sm[,"timestamp"] )

      SS = sm[ which( is.finite(sm$nss.b0) ) ,]
		  oo = which(!is.finite( SS$plon+SS$plat ) )
      if (length(oo)>0) SS = SS[ -oo , ]  # a required field for spatial interpolation
      save(SS, file=fn, compress=T )
      return ( "Done" )
    }

  }



