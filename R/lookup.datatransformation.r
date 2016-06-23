
  lookup.datatransformation = function( db ) {

    # determine data transformations based upon category of data and data source

    if (db=="snowcrab") {
      log.transform = bio.indicators::variable.list.expand("log.transform")
      scaled.centered = bio.indicators::variable.list.expand("scaled.centered")
      sn = bio.indicators::variable.list.expand("all.data")
      set = bio.snowcrab::snowcrab.db(DS="set.merge.cat") # base transform characteristics
      logs = bio.snowcrab::logbook.db(DS='logbook')
      repository = file.path( project.datadirectory("bio.snowcrab"), "R", "transform.lookup.rdata" )
    } else if (db=="groundfish") {
      log.transform = bio.indicators::variable.list.expand("log.transform")
      scaled.centered = bio.indicators::variable.list.expand("scaled.centered")
      sn = bio.indicators::variable.list.expand("all")
      set = bio.groundfish::groundfish.db( DS="set" )
	    logs=NULL
      repository = file.path( project.datadirectory("bio.groundfish"), "R", "transform.lookup.rdata" )
    } else {
      print(" Must define data sources for transformation to this function" )
      stop()
    }
     return( list( log.transform=log.transform, sn=sn, set=set,logs = logs, repository=repository ) )
  }


