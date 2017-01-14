
  metabolism.db = function( DS="", p=NULL, yr=NULL ) {

    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
      
      outdir = file.path( project.datadirectory("bio.indicators"), "metabolism" )

      dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
      fn = file.path( outdir, paste( "set.metabolism",  p$spatial.domain, p$taxa, p$season, ".rdata", sep=".") )

      if (DS=="metabolism") {
        set = NULL
        if (file.exists( fn) ) load( fn )
        return ( set )
      }

      set = survey.db( DS="set" ) # kg/km^2, no/km^2
      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2]
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]

      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
      }

      locsmap = match( 
        lbm::array_map( "xy->1", set[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams ) )

      set = cbind( set, indicators.lookup( p=p, DS="spatial", locsmap=locsmap ) )
      set = cbind( set, indicators.lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=set[,"timestamp"] ))
      set$t = indicators.lookup( p=p, DS="temperature",   locsmap=locsmap, timestamp=set[,"timestamp"] )

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
      oo = which(!is.finite( set$plon+set$plat ) )
      if (length(oo)>0)  set = set[ -oo, ]  # a required field for spatial interpolation
      set = lonlat2planar( set, proj.type=p$internal.projection )
      save( set, file=fn, compress=T )
      return (fn)
    }

  }


