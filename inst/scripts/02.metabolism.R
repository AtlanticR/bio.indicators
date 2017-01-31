
  # -----------------------------
  # estimate metabolic demand, given size structure
  p = bio.indicators::indicators.parameters( p=p, DS="metabolism")
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )
# o = bio.indicators::metabolism.db( DS="metabolism", p=p ) 
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }

