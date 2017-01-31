
indicators.map = function( ip=NULL, p=NULL, type="annual", voi=NULL ) {

  # ip is the first parameter passed in the parallel mode
  if (exists( "libs", p)) RLibrary( p$libs )
  if (is.null(ip)) ip = 1:p$nruns
 # over-ride default dependent variable name if it exists
  
  if (is.null(voi)) if (exists("variables",p)) if(exists("Y", p$variables)) voi=p$variables$Y
   
  loc = bathymetry.db(p=p, DS="baseline" )

  require( lattice )

  # -----------------------

  if ( type=="all" ) {

    allgrids = unique(c( p$spatial.domain.subareas, p$spatial.domain) )

    for ( gr in allgrids ) {
      print (gr)
      p1 = spatial_parameters(  p=p, type= gr )
      p1 = make.list( list( yrs=p1$yrs), Y=p1 )
      indicators.map( p=p1, type="climatology" ) # no parallel option .. just a few
      parallel.run( indicators.map, p=p1, type="annual" )
    }

  }

  # -----------------------


  if ( type %in% c("annual" ) ) {
    projectdir = file.path(p$project.root, "maps", voi, p$spatial.domain, "annual" )
    dir.create( projectdir, recursive=T, showWarnings=F )

    for (iy in ip ) {
      y = p$runs[iy, "yrs"]
      print(y)
      H = indicators.db( p=p, DS="predictions", yr=y, ret="mean" )

      if (is.null(H)) next ()
      xyz = cbind(loc, H[,iy])
      xyz = xyz[which( is.finite(rowSums(xyz))),]
 
      mapparams = indicators.lookup.mapparams( voi )
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        fn=outfn, loc=projectdir, 
        annot=mapparams$annot, at=mapparams$datarange , col.regions=mapparams$cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir,outfn))
    } 
  }


  # ------------------------------


  if ( type %in% c("climatology" ) ) {

    projectdir = file.path(p$project.root, "maps", voi, p$spatial.domain, "climatology" )
    dir.create( projectdir, recursive=T, showWarnings=F )
    dir.create( projectdir, recursive=T, showWarnings=F )

    H = indicators.db( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))
 
    for (vname in vnames ) {
      mapparams = indicators.lookup.mapparams( vnames)
      xyz = cbind(loc, H[,vname])
      xyz = xyz[which( is.finite(rowSums(xyz))),]
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        fn=outfn, loc=projectdir, 
        annot=mapparams$annot, at=mapparams$datarange , col.regions=mapparams$cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir,outfn))
    }

  }


  return (NULL)
}


