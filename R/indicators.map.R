
indicators.map = function( ip=NULL, p=NULL, type="all", voi=NULL ) {

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
    datarange = indicators.lookup.mapparams( "datarange", voi ) # hardcoded data ranges 
    cols = color.code( "blue.black", datarange )
    annot = gsub( ".", " ", toupper(voi), fixed=TRUE )
    
    for (iy in ip ) {
      y = p$runs[iy, "yrs"]
      print(y)
      H = indicators.db( p=p, DS="predictions", yr=y, ret="mean" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H[,iy])
      xyz = xyz[which( is.finite(rowSums(xyz))),]
      outfn = paste( voi, "mean", y, sep=".")
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=outfn, annot=annot, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 

      H = indicators.db( p=p, DS="predictions", yr=y, ret="sd" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H[,iy])
      xyz = xyz[which( is.finite(rowSums(xyz))),]
      outfn = paste( voi, "sd", y, sep=".")
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=outfn, annot=annot, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir,outfn))
    } 
  }


  # ------------------------------


  if ( type %in% c("climatology" ) ) {
    projectdir = file.path(p$project.root, "maps", voi, p$spatial.domain, "climatology" )
    dir.create( projectdir, recursive=T, showWarnings=F )
    H = indicators.db( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))
    
    for (vname in vnames ) {
      datarange = indicators.lookup.mapparams( "datarange", vnames) # hardcoded data ranges 
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(vname), fixed=TRUE )
      xyz = cbind(loc, H[,vname])
      xyz = xyz[which( is.finite(rowSums(xyz))),]
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=vname, annot=annot, at=datarange, col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir,outfn))
    }

  }

  return (NULL)
}


