
indicators.lookup = function( p, DS, locsmap=NULL, locs=NULL, timestamp=NULL, varnames=NULL, DB=NULL ) {

  if (0) { 
    # example of how to use this:
    set = survey.db( p=p, DS="set" )
    newvars = c("tmean", "tmap")
    locsmap = match( 
      lbm::array_map( "xy->1", set[,c("plon","plat")], gridparams=p$gridparams ), 
      lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams ) )
    
    # for spatial-only 
    sn = indicators.lookup( p=p, DS="spatial", locsmap=locsmap, varnames=newvars )
    names( sn ) = newvars
    set = cbind( set,  sn )

    # for space-time(year)
    sn = indicators.lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=set[,"timestamp"], varnames=newvars )
    names( sn  ) = newvars
    set = cbind( set,  sn )
  }


  if (is.null(locsmap)){
    grid = lbm::array_map( "xy->1", locs, gridparams=p$gridparams )
    baid = lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams )
    locsmap = match( grid, baid )
  }

  if (DS=="spatial"){
    if (is.null(DB)) DB = indicators.db(p=p, DS="spatial")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    out = DB[locsmap,varnames]
    return(out)
  }

  if (DS=="spatial.annual"){ 
    out = NULL
    if (!exists("tyears", p)) p$tyears = bio.temperature::temperature.parameters( current.year=current.year )$tyears
    dindex = cbind(locsmap, match( lubridate::year(timestamp), p$tyears ) )
    if (is.null(DB)) DB = indicators.db(p=p, DS="spatial.annual")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    for (vn in varnames){
      out = cbind( out, DB[[vn]][dindex] )
    }
    out = data.frame(out)
    names(out) =  varnames 
    return(out)
  }

  if (DS=="spatial.annual.seasonal"){ 
    # only temp for now
    if (!exists("tyears", p)) p$tyears = bio.temperature::temperature.parameters( current.year=current.year )$tyears
    out = NULL
    yrs = lubridate::year(timestamp)
    dyear = lubridate::decimal_date( timestamp ) -yrs
    dyear_index = as.numeric( cut( dyear, breaks=p$dyears, include.lowest=T, ordered_result=TRUE ) )
    dindex = cbind(locsmap, match( yrs, p$tyears ), dyear_index ) # check this
    if (is.null(DB)) DB=indicators.db(p=p, DS="spatial.annual.seasonal") # at this point this is the only database with seasonality .. other stats (than mean) will require supplemntary functionss
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    out = DB[dindex] 
    return(out)
  }


  if (DS=="baseline"){ 
    # all interpolated fields
    if (!exists("tyears", p)) p$tyears = bio.temperature::temperature.parameters( current.year=current.year )$tyears
    out = NULL
    dindex = cbind(locsmap, match( lubridate::year(timestamp), p$tyears ) )
    DB = indicators.db(p=p, DS="baseline")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    for (vn in varnames){
      out = cbind( out, DB[[vn]][dindex] )
    }
    
    return(out)
  }


}
