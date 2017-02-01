
indicators.lookup.mapparams = function( DS="datarange", vn ){

  if (DS=="datarange") {
    if ( grepl("ca1", vn) ) datarange = seq(-2, 2, length.out=100)
    if ( grepl("ca2", vn) ) datarange = seq(-2, 2, length.out=100)
    if ( grepl("pca1", vn) ) datarange = seq(-2, 2, length.out=100)
    if ( grepl("pca2", vn) ) datarange = seq(-2, 2, length.out=100)
    return(datarange)
  }

  # -------------------------

  if (DS==color.code) {
    return ("blue.black")
  }
  
}
