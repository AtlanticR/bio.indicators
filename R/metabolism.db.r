
  metabolism.db = function( DS="", p=NULL ) {

    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
      
      outdir = file.path( project.datadirectory("bio.indicators"), "metabolism" )

      dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
      fn = file.path( outdir, paste( "set.metabolism",  p$spatial.domain, p$taxa, ".rdata", sep=".") )

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
      set = lonlat2planar( set, proj.type=p$internal.projection )
      oo = which(!is.finite( set$plon+set$plat ) )
      if (length(oo)>0)  set = set[ -oo, ]  # a required field for spatial interpolation

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
      save( set, file=fn, compress=T )
      return (fn)
    }

  }


