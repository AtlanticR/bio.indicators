
  condition.db = function( DS="", p=NULL ) {

    if (DS %in% c( "condition", "condition.redo" ) ) {
     
      outdir = file.path( project.datadirectory("bio.indicators"), "condition" )
     
      dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
      infix = paste(p$spatial.domain, sep=".")
      fn = file.path( outdir, paste("set.condition", infix, "rdata", sep=".") )

      if (DS=="condition") {
        set = NULL
        if (file.exists( fn) ) load( fn )
        return ( set )
      }

      set = survey.db( DS="set" ) # kg/km^2, no/km^2

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2]
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yrs) , ]

      oo = which(!is.finite( set$plon+set$plat ) )
      if (length(oo)>0)  set = set[ -oo, ]  # a required field for spatial interpolation

      set = lonlat2planar( set, proj.type=p$internal.projection )

      # match sets and other data sources
      det = survey.db( DS="det" ) # kg/km^2, no/km^2
      det = det[ which( det$id %in% unique( set$id) ), ]
      det = det[, c("id", "spec_bio", "residual", "cfdet" ) ]

      # merge in taxa-specifc condition estimates ....
      sm = set[, c("id", "yr" )]  # # yr is a dummy variable so that sm remains a data frame
      for (tx in p$varstomodel ) {
        ii = taxonomy.filter.taxa( det$spec_bio, taxafilter=tx )
        if (is.null ( ii) ) {
          sm[,tx]= NA
          next()
        }
        smd = applyMean( det[ ii, c("id", "residual", "cfdet")] )
        names(smd) = c( "id", tx )
        sm = merge( sm, smd, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }

      sm$yr = NULL # dummy var

      set = merge( set, sm, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c("", ".sm") )
      save( set, file=fn, compress=T )
      return (fn)
    }
  }


