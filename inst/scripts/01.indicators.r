
 
  ## NOTE resolution is fixed at SSE

  current.year = 2016

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )


  # ----------------------------------------------------------
  # preform/prepare some lookup tables for faster lbm processing and generic lookups
  indicators.db( DS="spatial.redo", p=p ) 
  indicators.db( DS="spatial.annual.redo", p=p ) 
  indicators.db( DS="spatial.annual.seasonal.redo", p=p ) 

  
  # ----------------------------------------------------------
  # glue biological data sets together from various surveys
  # load and glue data together
  if (redo.source.data) {
    # these are here to show the dependencies of survey.db()
    bio.groundfish::groundfish.db( "set.base.redo" )
    bio.groundfish::groundfish.db( "cat.base.redo" ) 
    bio.groundfish::groundfish.db( "det.redo" )
    bio.snowcrab::snowcrab.db( DS ="set.clean.redo" )
    bio.snowcrab::snowcrab.db( DS ="cat.georeferenced.redo" ) 
    bio.snowcrab::snowcrab.db( DS ="det.georeferenced.redo" )
  }

  p = bio.indicators::indicators.parameters( p=p, DS="survey" )
  survey.db( DS="set.init.redo", p=p )
  survey.db( DS="cat.init.redo", p=p )
  survey.db( DS="det.init.redo", p=p )

  # the following requires the preformed indicators.db() (above) for lookups to complete the data
  survey.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  survey.db( DS="length.weight.redo", p=p  )  # # TODO:: parallelize me ... update the lcoal tables (not necessary)
  survey.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking

  figure.bio.map.survey.locations(p=p)  # see mpa/src/_Rfunctions/figure.trawl.density for more control
 
  indicators.db( DS="prediction.surface.redo", p=p ) 



 
  # -----------------------------
  # ordination
  vn ="ca1"
  current.year = 2016
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )

  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )
  for ( vn in p$varstomodel) {
    print(vn)
    
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    p = lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # the interpolation
    #   p = lbm( p=p, tasks=c( "stage0" ) )
#   p = lbm( p=p, tasks=c( "continue" ) )      
    p = lbm( p=p, tasks=c( "stage1" ) ) #  24 hrs 
    p = lbm( p=p, tasks=c( "stage2" ) ) #   3.5 hrs
    p = lbm( p=p, tasks=c( "save" ) )
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db ( DS="complete.redo", p=p )
    indicators.map( p=p  )
    gc()
  }


   global_model = lbm_db( p=p, DS="global_model") 
   summary( global_model )
   plot(global_model)

Formula:
ca1 ~ s(yr) + s(dyear, k = 3, bs = "tp") + s(yr, dyear, k = 30, 
    bs = "tp") + s(t, bs = "tp") + s(tmean, bs = "tp") + s(tamplitude, 
    bs = "tp") + s(z, bs = "tp") + s(dZ, bs = "tp") + s(ddZ, 
    bs = "tp") + s(log.substrate.grainsize, bs = "tp")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.026685   0.003278   8.141 4.14e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df       F  p-value    
s(yr)                       4.470  4.635   0.872    0.373    
s(dyear)                    2.000  2.000 800.572  < 2e-16 ***
s(yr,dyear)                23.581 27.000  55.034  < 2e-16 ***
s(t)                        8.517  8.933 168.265  < 2e-16 ***
s(tmean)                    7.706  8.578 510.319  < 2e-16 ***
s(tamplitude)               2.475  3.086  13.918 3.48e-09 ***
s(z)                        6.216  7.377  69.805  < 2e-16 ***
s(dZ)                       8.146  8.800   5.349 1.08e-06 ***
s(ddZ)                      8.426  8.904  10.101 1.70e-15 ***
s(log.substrate.grainsize)  7.442  8.399  46.699  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.786   Deviance explained = 78.7%
GCV = 0.22628  Scale est. = 0.22541   n = 20979
---




  # ----------------------------------------------------------
  # all landings

  p = bio.indicators::indicators.parameters( p=p, DS="landings" )
  bio.indicators::landings.db( DS="odbc", p=p ) # NOTE: run on MSWindows
  if (0) {
    # not yet ready
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }
  }



  # ----------------------------------------------------------
  # estimate condition
  p = bio.indicators::indicators.parameters( p=p, DS="condition" )
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute
# o = bio.indicators::condition.db( DS="condition", p=p )
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



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



  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size
  p = bio.indicators::indicators.parameters( p=p, DS="sizespectrum" )
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes
# o = bio.indicators::sizespectrum.db( DS="sizespectrum", p=p )
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
  p = bio.indicators::indicators.parameters( p=p, DS="speciesarea" )
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates
# o = bio.indicators::speciesarea.db( DS="speciesarea", p=p ) 
    for ( vn in p$varstomodel) {
      print(vn)
      p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
      p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
      indicators.db ( DS="complete.redo", p=p )
      indicators.map( p=p  )
      gc()
    }



 # -----------------------------
 # o = bio.indicators::biochem.db( DS="biochem", p=p )
    # for ( vn in p$varstomodel) {
    #   print(vn)
    #   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    #   p = lbm( p=p, DATA='indicators.db( p=p, DS="lbm_inputs" )' ) # the interpolation
    #   indicators.db ( DS="complete.redo", p=p )
    #   indicators.map( p=p  )
    #   gc()
    # }






