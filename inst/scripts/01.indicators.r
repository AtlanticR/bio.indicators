
  # glue biological data sets together from various surveys
  p = list( project.name = "survey" )
  p = bio.indicators::indicators.parameters( DS="survey", p=p )

  # load and glue data together
  survey.db( DS="set.init.redo", p=p )
  survey.db( DS="cat.init.redo", p=p )
  survey.db( DS="det.init.redo", p=p )
  survey.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
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


  ######################################
  # all landings
  ######################################
  marfissci.get.data(save.csv=FALSE)
    # TODO:: create into a landings.db approach



  ######################################
  # survey data assimilation complete.
  # now, generate indicators of interest from survey data
  ######################################

  p = list(project.name = "indicators" )

  # -----------------------------
  # estimate condition
  p = bio.indicators::indicators.parameters( DS="condition", p=p)
  bio.indicators::condition.db( DS="condition.redo", p=p ) # takes a minute

  # -----------------------------
  # estimate metabolic demand, given size structure
  p = bio.indicators::indicators.parameters( DS="metabolism", p=p)
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )

  # -----------------------------
  # analysis and spatial database of normalised size spectrum, average size
  p = bio.indicators::indicators.parameters( DS="sizespectrum", p=p)
  bio.indicators::sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  bio.indicators::sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
  bio.indicators::sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes

  # -----------------------------
  # count and record rarification curves from all available data --- refresh "survey.db" ~/ecomod/bio/src/bio.r
  p = bio.indicators::indicators.parameters( DS="speciesarea", p=p)
  bio.indicators::speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  bio.indicators::speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  bio.indicators::speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates

 # -----------------------------
   # ordination
  p = bio.indicators::indicators.parameters( DS="speciescomposition", p=p)
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )

 # -----------------------------
 # Used for merging back into survey.db as the 'higher level indicators have not yet been created/updated
  p = list( project.name = "habitat" )
  # p$yearstomodel = 1970:2015  --- change this

  p = bio.indicators::indicators.parameters( DS=p$project.name, p=p)
  indicators.db( DS="baseline.redo", p=p ) ## Time-invariant data (depth, substate, etc)
  lut = habitat.xyz.to.grid ( p, redo=TRUE ) # redo lookup table to convert xyz data to matrix/grid format

  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep("localhost", detectCores() )
  # p$clusters = c( rep( "nyx", 24), rep("tartarus", 24), rep("kaos", 24 ) )

  p = make.list( list( yrs=p$yearstomodel), Y=p )
  #parallel.run( indicators.db, DS="environmentals.redo", p=p ) #MG parallel isn't running properly at the moment
  indicators.db( DS="environmentals.redo", p=p )

