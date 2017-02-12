
indicators.map = function( ip=NULL, p=NULL, type="all", voi=NULL ) {

  # ip is the first parameter passed in the parallel mode
  if (exists( "libs", p)) RLibrary( p$libs )
   
  if (is.null(voi)) if (exists("variables",p)) if(exists("Y", p$variables)) voi=p$variables$Y
   
  require( lattice )

  # -----------------------

  if ( type=="all" ) {

    allgrids = unique(c( p$spatial.domain.subareas, p$spatial.domain) )

    for ( gr in allgrids ) {
      print (gr)
      p1 = spatial_parameters(  p=p, type= gr )
      p1 = make.list( list( yrs=p1$yrs), Y=p1 )
      indicators.map( p=p1, type="climatology" ) # no parallel option .. just a few
      parallel.run( indicators.map, p=p1, type="annual", voi=voi )
    }

  }

  # -----------------------


  if ( type %in% c("annual" ) ) {
    projectdir = file.path(p$project.root, "maps", voi, p$spatial.domain, "annual" )
    dir.create( projectdir, recursive=T, showWarnings=F )
    
    if (is.null(ip)) ip = 1:p$nruns
   
    # over-ride default dependent variable name if it exists
  
    loc = bathymetry.db(p=p, DS="baseline" )

    for (iy in ip ) {
      y = p$runs[iy, "yrs"]
      print(y)
      H = indicators.db( p=p, DS="predictions", year=y, ret="mean" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H)
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]
      datarange = indicators.lookup.mapparams( DS="datarange", voi ) # hardcoded data ranges 
      if (is.null(datarange)) datarange=quantile(xyz[,3], probs=c(0.05,0.95), na.rm=TRUE) 
      datarange = seq( datarange[1], datarange[2], length.out=100 )
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(voi), fixed=TRUE )
      outfn = paste( voi, "mean", y, sep=".")
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=outfn, annot=annot, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 

      H = indicators.db( p=p, DS="predictions", year=y, ret="sd" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H)
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]
      datarange = indicators.lookup.mapparams( DS="datarange", voi ) # hardcoded data ranges 
      if (is.null(datarange)) datarange=quantile(xyz[,3], probs=c(0.005,0.995), na.rm=TRUE) 
      datarange = seq( datarange[1], datarange[2], length.out=100 )
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(voi), fixed=TRUE )
      outfn = paste( voi, "sd", y, sep=".")

      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=outfn, annot=annot, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir, outfn))
    } 
    
  }


  # ------------------------------


  if ( type %in% c("climatology" ) ) {
    projectdir = file.path(p$project.root, "maps", voi, p$spatial.domain, "climatology" )
    dir.create( projectdir, recursive=T, showWarnings=F )
    
    loc = bathymetry.db(p=p, DS="baseline" )

    H = indicators.db( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))
    
    for (vn in vnames ) {
      xyz = cbind(loc, H[,vn])
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]
      datarange= NULL
      datarange = indicators.lookup.mapparams( DS="datarange", vn) # hardcoded data ranges 
      if (is.null(datarange)) datarange=quantile(xyz[,3], probs=c(0.005,0.995), na.rm=TRUE) 
      datarange = seq( datarange[1], datarange[2], length.out=100 )
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(vn), fixed=TRUE )
      bio.spacetime::map( xyz=xyz, cfa.regions=FALSE, depthcontours=TRUE, pts=NULL, 
        loc=projectdir, fn=vn, annot=annot, at=datarange, col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( projectdir, vn))
    }

  }

  return (NULL)
}


