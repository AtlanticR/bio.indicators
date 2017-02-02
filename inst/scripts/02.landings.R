 # ----------------------------------------------------------
  # all landings --do not want to interpolate this.. rather aggregate into meaningful areas, model patterns ans map

  p = bio.indicators::indicators.parameters( p=p, DS="landings" )
  bio.indicators::landings.db( DS="odbc", p=p ) # NOTE: run on MSWindows
  if (0) {
    # not yet ready
    for ( vn in p$varstomodel) {
      print(vn)
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }
  }

