
indicators.lookup = function( DS, yr=NULL, p=NULL ) {

  # define and load starting indicators.db source from which to do the lookup
  H = NULL

  if ( DS %in% c("speciesarea", "sizespectrum", "metabolism","speciescomposition", "condition", "biochem" ) ) {
    p$project.name = DS
    p$project.outdir.root = project.datadirectory( "bio.habitat", p$project.name, "analysis" )
    H = bio.habitat::habitat.interpolate( DS=DS, p=p )
    H = H[,  c("plon", "plat", p$varstomodel ) ]  # add variable names
  }

  if ( DS %in% c("depth", "depth.complete") ) {
    H = bio.bathymetry::bathymetry.db( p=p,  DS="complete", return.format = "dataframe" )
  }

  if ( DS=="substrate") {
    H = bio.substrate::substrate.db ( p=p, DS="planar")
    H$substrate.mean = log(H$grainsize)
    H = H[, c("plon", "plat", "substrate.mean") ]
  }

  if ( DS %in% c( "temperature.complete", "temperature.annual", "temperature", "temperature.seasonal", "temperature.climatology" ) ) {
    if (is.null(yr)) {
      # choose one,, any will do
      if (exists("tyears", p)) yr = p$tyears[1]
      if (exists("tyears.climatology", p)) yr = p$tyears.climatology[1]
    }
    if ( DS %in% c("temperature.climatology" ) ) {
      H = bio.temperature::temperature.db( p=p, DS="complete", year=yr  )
      H = H[, c("plon", "plat", "tmean.cl", "tamp.cl", "wmin.cl", "thp.cl", "tsd.cl" )]
    }
    if ( DS %in% c( "temperature.annual", "temperature", "temperature.seasonal" ) ) {
      # note this is valid only for the default spatial domain .. so must regrid
      H = bio.temperature::temperature.db( p=p, DS="spacetime.prediction", yr=yr  )
    }
    if ( DS %in% c( "temperature.complete" ) ) {
      H = bio.temperature::temperature.db( p=p, DS="complete", year=yr  )
      # nothing to do ... keep all
    }
  }

  if ( DS %in% c("default", "environmentals", "time.variant" ) ) {
    H = bio.indicators::indicators.db( DS="environmentals", p=p, year=yr )
  }

  if ( DS %in% c("all", "all.data") ) {
    H = bio.indicators::indicators.db( DS="complete", p=p, year=yr )
  }

  if ( DS %in% c("baseline", "time.invariant") ) {
    H = bio.indicators::indicators.db( DS="baseline", p=p )
  }

  if (is.null(H)) {
    if(DS=="temperature")stop("returned baseline data NOT TEMPERATURE")
    H = bio.indicators::indicators.db( DS="baseline", p=p ) # default for most, including depth, substrate, etc
  }

  return( H )
}


