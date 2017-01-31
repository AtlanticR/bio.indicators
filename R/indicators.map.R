
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
      indicators.map( p=p1, type="lbm.stats" ) # no parallel option .. just a few
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
      H = indicators.db( p=p, DS="bottom.statistics.annual", yr=y, ret=vname )

      if (is.null(H)) next ()

      if (vname %in% c("tmean") ) {
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom", y, sep=".")
        annot = y
      } 

      if (vname %in% c("tsd") ) {
        datarange = seq(0.001, 6, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.sd", y, sep=".")
        annot = y
      } 
   
      if (vname %in% c("tmin") ) {
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.min", y, sep=".")
        annot = y
      }
   
      if (vname %in% c("tmax") ) {
        datarange = seq(-0.5, 10, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.max", y, sep=".")
        annot = y
      }

      if (vname %in% c("amplitude") ) {
        datarange = seq(0,5, length.out=100)
        cols = color.code( "blue.black", datarange )
        outfn = paste( "temperatures.bottom.amplitude", y, sep=".")
        annot = y
      }

      bio.spacetime::map( xyz=cbind(loc, H[,iy]), cfa.regions=F, depthcontours=T, pts=NULL, annot=annot,
        fn=outfn, loc=projectdir, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 

    } 
  }

  # ------------------------------

  if ( type %in% c("climatology" ) ) {

    projectdir = file.path( project.datadirectory("bio.temperature"), "maps", p$spatial.domain, "bottom.predictions", "climatology" )
    dir.create( bottomdir.maps, recursive=T, showWarnings=F )

    H = indicators.db( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))
 
    for (vname in vnames ) {

      if ( grepl("ca1", vname) ) {
        datarange = seq(-2, 2, length.out=100)
        annot = "CA1"
      }

      if ( grepl("ca2", vname) ) {
        datarange = seq(-2, 2, length.out=100)
        annot = "CA1"
      } 

      if ( grepl("pca1", vname) ) {
        datarange = seq(-2, 2, length.out=100)
        annot = "PCA1"
      }

      if ( grepl("pca2", vname) ) {
        datarange = seq(-2, 2, length.out=100)
        annot = "PCA1"
      }

      if ( grepl("mean.climatology", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "mean.climatology", sep=".")
        annot = paste( annot, "Mean climatology\n", sep=" ")
      } 

      if ( grepl("sd.climatology", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "sd.climatology", sep=".")
        annot = paste( annot, "SD climatology\n", sep=" ")
      } 
   
      if (grepl("sdTotal", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "sdTotal", sep=".")
        annot = paste( annot, "sdTotal\n", sep=" ")
        xyz=cbind(loc, H[,vname])
      } 

      if (grepl("rsquared", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "rsquared", sep=".")
        annot = paste( annot, "rsquared\n", sep=" ")
        xyz=cbind(loc, H[,vname])
      } 
   
      if (grepl("ndata", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "ndata", sep=".")
        annot = paste( annot, "ndata\n",  sep=" ")
        xyz=cbind(loc, H[,vname])
      }
   
      if (grepl("sdSpatial", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "sdSpatial", sep=".")
        annot = paste( annot, "sdSpatial\n",  sep=" ")
        xyz=cbind(loc, H[,vname])
     }

      if (grepl("sdObs", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "sdObs", sep=".")
        annot = paste( annot, "sdObs\n",  sep=" ")
        xyz=cbind(loc, (H[,vname]))
      }

      if (grepl("range", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "range", sep=".")
        annot = paste( annot, "range\n",  sep=" ")
        xyz=cbind(loc, log(H[,vname]))
        xyz = xyz[which( is.finite(rowSums(xyz))),]
      }

      if (grepl("phi", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "phi", sep=".")
        annot = paste( annot, "phi\n",  sep=" ")
        xyz=cbind(loc, log(H[,vname]))
        xyz = xyz[which( is.finite(rowSums(xyz))),]
      }

      if (grepl("nu", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "nu", sep=".")
        annot = paste( annot, "nu\n",  sep=" ")
        xyz=cbind(loc, (H[,vname]))
      }

      if (grepl("ar_timerange", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "ar_timerange", sep=".")
        annot = paste( annot, "ar_timerange\n",  sep=" ")
        xyz=cbind(loc, (H[,vname]))
      }

      if (grepl("ar_1", vname) ) {
        cols = color.code( "blue.black", datarange )
        outfn = paste( annot, "ar_1", sep=".")
        annot = paste( annot, "ar_1\n",  sep=" ")
        xyz=cbind(loc, H[,vname])
      }

      bio.spacetime::map( xyz=cbind(loc, H[,which( p$bstats==vname)]), cfa.regions=F, depthcontours=T, pts=NULL, annot=annot,
        fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols,
        corners=p$corners, spatial.domain=p$spatial.domain ) 
      print( file.path( bottomdir.maps,outfn))
    }

  }


  return (NULL)
}





