
indicators.lookup.mapparams = function( vname ){

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
    } 

    if (grepl("rsquared", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "rsquared", sep=".")
      annot = paste( annot, "rsquared\n", sep=" ")
    } 
 
    if (grepl("ndata", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "ndata", sep=".")
      annot = paste( annot, "ndata\n",  sep=" ")
    }
 
    if (grepl("sdSpatial", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "sdSpatial", sep=".")
      annot = paste( annot, "sdSpatial\n",  sep=" ")
   }

    if (grepl("sdObs", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "sdObs", sep=".")
      annot = paste( annot, "sdObs\n",  sep=" ")
    }

    if (grepl("range", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "range", sep=".")
      annot = paste( annot, "range\n",  sep=" ")
    }

    if (grepl("phi", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "phi", sep=".")
      annot = paste( annot, "phi\n",  sep=" ")
    }

    if (grepl("nu", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "nu", sep=".")
      annot = paste( annot, "nu\n",  sep=" ")
    }

    if (grepl("ar_timerange", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "ar_timerange", sep=".")
      annot = paste( annot, "ar_timerange\n",  sep=" ")
    }

    if (grepl("ar_1", vname) ) {
      cols = color.code( "blue.black", datarange )
      outfn = paste( annot, "ar_1", sep=".")
      annot = paste( annot, "ar_1\n",  sep=" ")
    }

    return( list( datarange=datarange, cols=cols, outfn=outfn, annot=annot ))

}
