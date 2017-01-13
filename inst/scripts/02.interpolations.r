
# Generic spatio-temporal interpolations and maping of data
# using the interpolating functions and models defined in bio.indicators / lbm
# NOTE:: could loop this and make automatic but ...
#        because it is so slow right now manually choosing selecting is more reliable (crashes due to power outage, etc ..)
# also, it becomes relatively simple to add other indicators using this approach ..
    
    current.year = 2016
 
    ## NOTE resolution is fixed at SSE
    projectstointerpolate = c("speciescomposition", "metabolism", "condition", "speciesarea", "sizespectrum" )

    for (project.name in projectstointerpolate ) {  
      print (project.name)
      p = NULL
      p = bio.indicators::indicators.parameters( DS=project.name, current.year=current.year )
      for ( vn in p$varstomodel) {
        print(vn)
        p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
        p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
        indicators.db ( DS="complete.redo", p=p )
        indicators.map( p=p  )
        gc()
      }
    }

   
    # TODO :: biologicals begin in 1970 ..  need to fix
    #        .. at present data from 1970 are copied to all pre 1970 data years

  







