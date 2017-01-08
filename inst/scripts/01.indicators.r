
  # glue biological data sets together from various surveys

  p = bio.indicators::indicators.parameters( DS="survey" )

  # load and glue data together
  survey.db( DS="set.init.redo", p=p )
  survey.db( DS="cat.init.redo", p=p )
  survey.db( DS="det.init.redo", p=p )

  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"

  # TODO:: parallelize me ...
  lengthweight.db( DS="update", p=p  )  # update the lcoal tables (not necessary)

  survey.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking

  # generic plots
  figure.bio.map.survey.locations()  # see mpa/src/_Rfunctions/figure.trawl.density for more control

    #  --- look in metabolism functions and complexity/condition

    # to obtain stats from l-w relationships used to impute mass/leng and estimate condition
    # a = length.weight.regression ( DS="parameters", p=p )

    # to obtain biomass estimates after correction for tow, etc.
    # a = biomass.estimation (DS="saved"", p=p )


  # ----------------------------------------------------------
  # all landings

  p = bio.indicators::indicators.parameters( DS="landings" )
  bio.indicators::landings.db( DS="odbc", p=p ) # NOTE: run on MSWindows


  # ----------------------------------------------------------
  # survey data assimilation complete.
  # now, generate indicators of interest from survey data


  # ----------------------------------------------------------
  # estimate condition
  p = bio.indicators::indicators.parameters( DS="condition" )
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute
# o = bio.indicators::condition.db( DS="condition", p=p )



  # -----------------------------
  # estimate metabolic demand, given size structure
  p = bio.indicators::indicators.parameters( DS="metabolism")
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )
# o = bio.indicators::metabolism.db( DS="metabolism", p=p ) 



  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size
  p = bio.indicators::indicators.parameters( DS="sizespectrum" )
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes
# o = bio.indicators::sizespectrum.db( DS="sizespectrum", p=p )



  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
  p = bio.indicators::indicators.parameters( DS="speciesarea" )
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates
# o = bio.indicators::speciesarea.db( DS="speciesarea", p=p ) 



 # -----------------------------
   # ordination
  p = bio.indicators::indicators.parameters( DS="speciescomposition" )
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )
# o = bio.indicators::speciescomposition.db( DS="speciescomposition", p=p )



 # -----------------------------
 # o = bio.indicators::biochem.db( DS="biochem", p=p )




 # -----------------------------
 # Glue it all together
  p = bio.indicators::indicators.parameters( DS="indicators" )
  p = make.list( list( yrs=p$yearstomodel), Y=p )
  
  indicators.db( DS="baseline.redo", p=p, dyear=p$prediction.dyear) 


