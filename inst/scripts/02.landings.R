 # ----------------------------------------------------------
  # all landings --do not want to interpolate this.. rather aggregate into meaningful areas, model patterns ans map

  current.year = 2016

  
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="landings" )
  bio.indicators::landings.db( DS="odbc", p=p ) # NOTE: run on MSWindows
  

  if (0) {
    # not yet ready for prime time
    for ( vn in p$varstomodel) {
      print(vn)
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }
  }

