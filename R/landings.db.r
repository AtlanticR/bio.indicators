landings.db = function(DS, p) {

  #\\wrapper around Mike McMahon's marfisci tools

  ldbdir = project.datadirectory("bio.indicators", "landings", "raw_data" )

  if (DS %in% c("odbc", "odbc.redo")) {
    if (DS=="odbc") {
      out = NULL
      for (yr in p$marfis.years) {
        fn = file.list( ldbdir, paste( "odbc", yr, "rdata", sep="." ))
        if ( file.exists(fn)) {
          ldb = NULL
          load(fn)
          out = rbind( out, ldb )
        }
      }
      return(out)
    }

    for (yr in p$marfis.years) {
      ldb = marfissci.get.data(years=yr, get.nonlandings=TRUE, save.csv=FALSE)
      fn = file.list( ldbdir, paste("odbc", yr, "rdata", sep="." ))
      save( ldb, file=fn, compress=TRUE )
      print(fn)
    }
    return(ldbdir)
  }


  if (DS %in% c("aggregated", "aggregated.redo") ) {



    test = marfissci.process.data(df, agg.minutes=2, agg.by="SPECIES_CODE", save.RDS=F)

    marfissci.batch.process()
    cl.batch.process()
    marfissci.simple.map()

  }



}
