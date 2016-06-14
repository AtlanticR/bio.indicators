
  habitat.xyz.to.grid = function( p, redo=FALSE ) {

    ddir = project.datadirectory( "bio.indicators", "analysis", "habitat" )

    fn = file.path( ddir, paste( "habitat.xyz2grid", p$spatial.domain, "rdata", sep="." ) )

    if (redo) {

      # bioLibrary("bio.bathymetry")  # base structure from baythmetry db
      H = bio.bathymetry::bathymetry.db( p=p, DS="baseline" )
      row = round(( H$plon - min(H$plon) ) / p$pres ) + 1
      col = round(( H$plat - min(H$plat) ) / p$pres ) + 1
      row.col = cbind(row, col)
      save ( row.col, file=fn, compress=TRUE )

    } else {

      load(fn)

    }

    return(row.col)
  }


