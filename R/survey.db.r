
  survey.db = function( p=NULL, DS=NULL, selection=NULL ) {
    #\\ assimilation of all survey data into a coherent form
    surveydir = project.datadirectory( "bio.indicators", "survey" )

    dir.create( surveydir, showWarnings=FALSE, recursive=TRUE )

    if (DS %in% c("set.init", "set.init.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( surveydir, "set.init.rdata"  )
      if (DS=="set.init") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }

			set.names =  c("data.source", "id", "timestamp", "yr", "lon", "lat",
                     "z", "t", "sal", "oxyml", "sa", "gear", "setquality" )

      if ( "groundfish" %in% p$data.sources ) {
        # settype:
        # 1=stratified random,
        # 2=regular survey,
        # 3=unrepresentative(net damage),
        # 4=representative sp recorded(but only part of total catch),
        # 5=comparative fishing experiment,
        # 6=tagging,
        # 7=mesh/gear studies,
        # 8=explorartory fishing,
        # 9=hydrography
        y = bio.groundfish::groundfish.db( "set.base" )
        y$data.source = "groundfish"
        y$sa = y$sweptarea  # sa is in km^2
        y$z = y$sdepth  # m
        y$gear = y$geardesc
        y$setquality = NA
        y$setquality[ which( y$settype %in% c(1,2,5) ) ] = "good"
        gsvn = c("data.source", "id", "timestamp", "yr", "lon", "lat",
                 "z", "temp", "sal", "oxyml", "sa", "gear", "setquality")
        set = rbind( set, y[ ,gsvn ] )
        names(set) = set.names
        rm (y); gc()
      }

      if ( "snowcrab" %in% p$data.sources ) {
        y =  bio.snowcrab::snowcrab.db( DS ="set.clean" )
        y$data.source = "snowcrab"
        y$gear ="Nephrops trawl"
        y$id = paste( y$trip, y$set, sep="." )
        y$setquality = NA
        y$setquality[ which( y$towquality == 1 ) ] = "good"  # 1=good
        y$sal = NA  # dummy
        y$oxyml = NA # dummy var

        set = rbind( set, y[ , set.names ] )  # sa is in km^2
        rm (y); gc()
      }

      save( set, file=fn, compress=T )
      return (fn)
    }


    # --------------------


    if (DS %in% c("cat.init", "cat.init.redo") ) {
      # all species caught
      cat = NULL # trip/cat loc information
      fn = file.path( surveydir, "cat.init.rdata"  )
      if (DS=="cat.init") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }

      ###  NOTE:: cf == correction factor is a reweighting required to make each totno and totmass comparable for each set and species subsampling

      cat.names =  c("data.source", "id", "spec", "spec_bio", "totno", "totmass" )
      if ( "groundfish" %in% p$data.sources ) {

        x = bio.groundfish::groundfish.db( "cat.base" )  #kg/set, no/set

        x$data.source = "groundfish"
        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )

        x$totnumber = NA
        x$totmass = NA
        # netmesuration and/or positional information based sa estimates for WIIa net
        isa = which(is.finite( x$sweptarea))
        x$totnumber[isa] = x$totno[isa] / x$sweptarea[isa]
        x$totmass[isa] = x$totwgt[isa] / x$sweptarea[isa]

        # if without sweptarea, most likely another gear, use SA based upon positional info: sakm2
        isana = which(! is.finite( x$sweptarea))
        x$totnumber[isana] = x$totno[isana] / x$sakm2[isana]
        x$totmass[isana] = x$totwgt[isana] / x$sakm2[isana]

        sa.gear =  tapply( x$sakm2, x$geardesc, mean, na.rm=TRUE )
        isnaz = which( !is.finite( x$totmass ) )
        if (length(isnaz) > 0) {
          sa.estim = sa.gear[x$geardesc[isnaz] ]
          x$totmass[isnaz] = x$totwgt[isnaz] / x$sakm2[isnaz]
        }

        isnaz = which( !is.finite( x$totnumber ) )
        if (length(isnaz) > 0) {
          sa.estim = sa.gear[x$geardesc[isnaz] ]
          x$totnumber[isnaz] = x$totno[isnaz] / x$sakm2[isnaz]
        }

        x$totno = x$totnumber
        x = x[ which( x$itis.tsn > 0 ), ]
				x = x[, cat.names]
        cat = rbind( cat, x )
        rm (x); gc()
      }

      if ( "snowcrab" %in% p$data.sources ) {
        x = bio.snowcrab::snowcrab.db( DS ="cat.georeferenced" ) # sa corrected ; kg/km2; no./km2
        x$data.source = "snowcrab"
        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
        x$id = paste( x$trip, x$set, sep="." )
        x = x[, cat.names]
        iissp = taxonomy.recode( from="spec", to="parsimonious", tolookup=2526 ) # snow crab using groundfish codes
        oo = which( !is.finite(x$totno) & x$spec_bio==iissp  )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totno[oo] = 0
        oo = which( !is.finite(x$totmass) & x$spec_bio== iissp )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totmass[oo] = 0
				cat = rbind( cat, x  )
        rm (x); gc()
      }

      cat$id2 = paste( cat$id, cat$spec_bio, sep="." )

      save( cat, file=fn, compress=T )
      return (fn)
    }



    # --------------------


    if (DS %in% c("det.init","det.init.redo") ) {
      # all species caught
      det = NULL # biologicals
      fn = file.path( surveydir, "det.init.rdata"  )
      if (DS=="det.init") {
        if (file.exists( fn) ) load( fn)
        return ( det )
      }


        # sex codes
        #  male = 0
        #  female = 1
        #  sex.unknown = 2

        # maturity codes
        #  immature = 0
        #  mature = 1
        #  mat.unknown = 2


      det.names =  c("data.source", "id", "spec", "spec_bio", "detid", "sex", "mass", "len", "mat" )
      if ( "groundfish" %in% p$data.sources ) {
        x = bio.groundfish::groundfish.db( "det" )
        x$data.source = "groundfish"
        x$detid = x$fshno

        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )


        # mass in kg, len in cm

        # convert sex codes to snow crab standard
        # --------- codes ----------------
        # sex: 0=undetermined, 1=male, 2=female,  3=hermaphrodite, 9= not examined
        # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature),
        #      5=spawning(running), 6=spent, 7=recovering, 8=resting
        # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
        #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
        #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
        # --------- codes ----------------
  
        u = which(x$spec==2526)
        if ( mean(x$len[u], na.rm=TRUE) > 20 ) {
          # 200 mm or 20 cm is really the upper limit of what is possible for snow crab (in 2016, it was 50)
          # if the mean is above this then there is an issue, assume it is recorded as mm
          # and convert to cm as that is the expectation in groundfish.db and indicators.db 
          message( "groundfish gsdet seems to have stored snowcrab lengths in mm ? -- please check")
          x$len[u] = x$len[u] / 10
        }

        sx = x$sex
        x$sex = NA
        oo = which( sx %in% c(0, 3, 9) ); if (length(oo)>0) x$sex[oo] = 2 # unknown
        oo = which( sx %in% c(1) ); if (length(oo)>0) x$sex[oo] = 0 # male
        oo = which( sx %in% c(2) ); if (length(oo)>0) x$sex[oo] = 1 # female

        # convert maturity to snow crab standard
        mt = x$mat
        x$mat = NA
        oo = which( mt %in% c(0) ); if (length(oo)>0) x$mat[oo] = 2 # unknown
        oo = which( mt %in% c(1) ); if (length(oo)>0) x$mat[oo] = 0  # immature
        oo = which( mt %in% c(2,3,4,5,6,7,8) ); if (length(oo)>0) x$mat[oo] = 1 # mature  -- investment into gonads has begun

        det = rbind( det, x[, det.names] )
        rm (x); gc()

      }

      if ( "snowcrab" %in% p$data.sources ) {

        x = bio.snowcrab::snowcrab.db( DS ="det.georeferenced" )
        x$data.source = "snowcrab"
        x$id = paste( x$trip, x$set, sep="." )
        x$spec = 2526
        x$spec_bio =  taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec ) # snow crab using groundfish codes
        x$detid = x$crabno
        x$len = x$cw / 10  # convert mm to cm
        # x$cfdet = 1/x$sa  ########## <<<<<< ------ NOTE THIS accounts only for SA as there is no subsampling (so far)
        x$sex = as.numeric( as.character( x$sex) )
        x$mat = as.numeric( as.character( x$mat) )
        x$mass = x$mass /1000  # g to kg

        det = rbind( det, x[, det.names] )
        rm (x); gc()
      }

      det$id2 = paste( det$id, det$spec_bio, sep=".")

      save( det, file=fn, compress=T )
      return (fn)
    }


    # -------------


    if (DS %in% c("set.intermediate", "set.intermediate.redo") ) {
      # lookup missing information
      
      set = NULL # trip/set loc information
      fn = file.path( surveydir, "set.intermediate.rdata"  )
      if (DS=="set.intermediate") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }

      set = survey.db( DS="set.init", p=p )
      set = set[ which(is.finite(set$lon + set$lat + set$yr ) ) , ]  #  fields are required
      oo =  which( !duplicated(set$id) )
      if (length(oo) > 0 ) set = set[ oo, ]
      set = lonlat2planar( set, proj.type=p$internal.projection )  # plon+plat required for lookups

      set$dyear = lubridate::decimal_date( set$timestamp ) - set$yr


      # merge depth
      iz = which( !is.finite(set$z) )
      if (length(iz) > 0) {
        set$z[iz] = bio.bathymetry::bathymetry.lookup( p=p, locs=set[iz, c("plon","plat")], vnames="z" )
      }
      set = set[ which(is.finite(set$z)), ] # depth is a required field

      # merge temperature
      it = which( !is.finite(set$t) )
      if (length(it) > 0) {
        set$t[it] = bio.temperature::temperature.lookup( p=p, locs=set[it, c("plon","plat")], timestamp=set$timestamp[it] )
      }
      set = set[ which(is.finite(set$t)), ] # temp is required

      set$oxysat = compute.oxygen.saturation( t.C=set$t, sal.ppt=set$sal, oxy.ml.l=set$oxyml)
     
      save( set, file=fn, compress=T )
      return (fn)
    }


    # ---------------------

    if (DS %in% c("lengthweight.redo", "lengthweight.parameters", "lengthweight.residuals") ) {

      ## TODO -- make parallel require(multicore)

      ddir = file.path( project.datadirectory("bio.indicators"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )

      fn = file.path( ddir, "bio.length.weight.parameters.rdata" )
      fn2 = file.path( ddir, "bio.length.weight.residuals.rdata" )

      if (DS=="lengthweight.parameters") {
        res = NULL
        if (file.exists( fn ) ) load( fn )
        return( res )
      }

      if (DS=="lengthweight.residuals") {
        lwr = NULL
        if (file.exists( fn2 ) ) load( fn2 )
        return( lwr )
      }

      # this mirrors the relevent changes/recoding in indicators.db("det")
      x = survey.db( DS="det.init", p=p )
      x$spec = x$spec_bio
      x = x[ which( is.finite( x$spec)), ]
      x$sex[ which( !is.finite(x$sex)) ] = 2 # set all uncertain sexes to one code sex code
      x$mat[ which( !is.finite(x$mat)) ] = 2 # set all uncertain sexes to one code sex code

      res = expand.grid(
        spec = sort( unique( x$spec )),
        sex = sort( unique( x$sex )),
        mat = sort( unique( x$mat ))
      )
      # sex codes (snowcrab standard)
      #  male = 0
      #  female = 1
      #  sex.unknown = 2

      # mat codes (snowcrab standard)
      #  imm = 0
      #  mat = 1
      #  mat.unknown = 2

      unknown = 2
      x$residual = NA

      # initialise new variables
      res$rsq = NA
      res$sigma = NA
      res$df = NA
      res$b0 = NA
      res$b1 = NA
      res$b0.se = NA
      res$b1.se = NA
      res$pvalue = NA
      for (i in 1:nrow(res)) {
        wsp = which( x$spec == res$spec[i] )
        if (length( wsp) < 10 ) next()
        # remove extremes for each species from the data to generate regressions
        ql = quantile( x$len[wsp], probs=c(0.005, 0.995), na.rm=T )
        qm = quantile( x$mass[wsp], probs=c(0.005, 0.995), na.rm=T )
        wqn =  which( x$len> ql[1] & x$len< ql[2] & x$mass> qm[1] & x$mass< qm[2] )
        wsx = which( x$sex==res$sex[i] )
        if (res$sex[i]==unknown) wsx = wsp  # use all possible data (not just unknowns) for this class (hermaphrodite/unsexed/unknown)
        wmt = which( x$mat==res$mat[i] )
        if (res$sex[i]==unknown) wsx = wsp  # use all possible data (not just unknowns) for this class (mat unkown)
        w = intersect( intersect( intersect( wsp, wqn ), wsx ), wmt )
        nw = length(w)
        if ( nw > 5 ) {
          q = x[w ,]
          q.lm = try( lm( log10(mass) ~ log10(len), data=q ) )
          if (class( q.lm) %in% "error" ) next()
          s = summary( q.lm )
          res$rsq[i] = s$r.squared
          res$sigma[i] = s$sigma
          res$df[i] = s$df[2]
          res$b0[i] = s$coefficients[1]
          res$b1[i] = s$coefficients[2]
          res$b0.se[i] = s$coefficients[3]
          res$b1.se[i] = s$coefficients[4]
          res$pvalue[i] = pf(s$fstatistic[1],s$fstatistic[2],s$fstatistic[3],lower.tail=FALSE)
          x$residual[w] = rstandard(q.lm)
          print( res[i,] )
        }
      }
      ooo = which( abs( x$residual ) > 4 )
      if (length(ooo) > 0 ) x$residual [ooo] = NA
      lwr = x
      save( lwr, file=fn2, compress=TRUE )
      save( res, file=fn, compress=TRUE )
      return( fn )
 
    }


    # --------------------


    if (DS %in% c("det","det.redo") ) {

      # error checking, imputation, etc

      det = NULL
      fn = file.path( surveydir, "det.rdata"  )
      if (DS=="det") {
        if (file.exists( fn) ) load( fn)
        return ( det )
      }

        # sex codes
        #  male = 0
        #  female = 1
        #  sex.unknown = 2

        # maturity codes
        #  immature = 0
        #  mature = 1
        #  mat.unknown = 2

      det = survey.db( DS="lengthweight.residuals", p=p )

      # fix mass, length estimates where possible using model parameters
      # try finest match first: by spec:mat, spec:sex, spec

      lwp = survey.db( DS="lengthweight.parameters", p=p )
      # note: lwp$spec is derived from spec_bio, as above

      ims = which( !is.finite( det$mass) )
      sps = sort( unique( det$spec_bio[ ims ] ) )
      mats = sort( unique( det$mat))
      sexes = sort( unique( det$sex))

      for (sp in sps) {
        isp = which( det$spec_bio == sp )

        # first try exact matches based upon {spec, mat, sex}
        for ( mat in mats ) {
        for ( sex in sexes ) {
          u = which( det$mat==mat & det$sex==sex & !is.finite(det$mass ) )
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$mat==mat & lwp$sex==sex & lwp$rsq>0.75)
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }}

        # next try exact matches based upon {spec, mat}
        for ( mat in mats ) {
          u = which( det$mat==mat & !is.finite(det$mass )  )
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$mat==mat & is.na(lwp$sex  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }

       # next try exact matches based upon {spec, sex}
        for ( sex in sexes ) {
          u = which( det$sex==sex & !is.finite(det$mass )  )
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$sex==sex & is.na(lwp$mat  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }

       # next try exact matches based upon {spec} only
          u = which( is.na(det$mass ))
          w = intersect( isp , u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & is.na(lwp$sex) & is.na(lwp$mat  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
      }

      # last go -- try exact matches based upon {spec} only but less contrained by rquared
      for (sp in sps) {
        isp = which( det$spec_bio == sp )
          u = which( is.na(det$mass ))
          w = intersect( isp , u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & is.na(lwp$sex) & is.na(lwp$mat ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
      }

      # estimate metabolic rates estimates (requires temperature estimate )
      set = survey.db( DS="set.intermediate", p=p  ) # kg/km^2, no/km^2
      set = set[ , c("id", "t")]  # temperature is required to estimate MR ..

      det = merge( det, set, by="id", all.x=T, all.y=F, sort=F )
      detmr = metabolic.rates ( det$mass * 1000, det$t )
      det = cbind( det, detmr )


      ## det$cfdet needs to be updated as it was formed without the above re-estimation of missing weights

      cat = survey.db( DS="cat.init", p=p )

      massTotCat = applySum( det[ ,c("id2", "mass")], newnames=c("id2","massTotdet" ) )

      noTotCat = applySum( det$id2, newnames=c("id2","noTotdet" ) )

      cat = merge( cat, massTotCat, by="id2", all.x=T, all.y=F, sort=F )  # set-->kg/km^2, det-->km
      cat$massTotdet[ which( !is.finite (cat$massTotdet ))] = 0  ### whenn missing it means no determinations were made
      cat = merge( cat, noTotCat, by="id2", all.x=T, all.y=F, sort=F )    # set-->no/km^2, det-->no
      cat$noTotdet[ which( !is.finite (cat$noTotdet ))] = 0  ### whenn missing it means no determinations were made


      cf = data.frame( cbind( cfdetm =  cat$totmass/ cat$massTotdet, cfdetn =  cat$totno/ cat$noTotdet ) )
      cf$rsids = NA
      cf$cfdetm [ which(cf$cfdetm==0) ] = NA
      cf$cfdetn [ which(cf$cfdetn==0) ] = NA
      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ))

      cfmod = lm( cfdetm ~ cfdetn, data=cf[cfi,] )
      cf$rsids[cfi] = rstandard( cfmod)

      # remove extremes and redo
      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ) & abs(cf$rsids) < 4 )
      cfmod = lm( cfdetm ~ cfdetn, data=cf[cfi,] )
      cf$rsids = NA
      cf$rsids[cfi] = rstandard( cfmod)

      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ) & abs(cf$rsids) < 3 )  #make even more selective

      cat$cfdet = NA
      cat$cfdet[cfi] = cat$totmass[cfi] / cat$massTotdet[cfi]   # totwgt already corrected for vessel and tow .. cfdet is the multiplier required to make each det measurement scale properly

      oo = which ( !is.finite( cf$cfdetm ) & is.finite(cf$cfdetn) )
      if (length(oo)>0) cat$cfdet[oo] = cf$cfdetn[oo]

      oo = which ( is.finite( cf$cfdetm ) & !is.finite(cf$cfdetn) )
      if (length(oo)>0) cat$cfdet[oo] = cf$cfdetm[oo]

      oo = which ( !is.finite( cat$cfdet ) )

      for ( ds in unique( cat$data.source[oo] ) ) {
        for ( sp in unique( cat$spec_bio[oo]  )) {

          mm = which( cat$data.source==ds & cat$spec_bio==sp & (!is.finite( cat$cfdet ) |  cat$cfdet==0 ) )
          nn = which( cat$data.source==ds & cat$spec_bio==sp )
          if ( length(mm)>0  & length(nn)>0  ) {
            cat$cfdet[ mm ] = median( cat$cfdet[nn] , na.rm=TRUE )
          }
      }}

      cat = cat[, c("id2", "cfdet")]  # a lot of missing values but this is normal as they will not be represented in "det"

      det$cfdet = NULL
      det = merge( det, cat, by="id2", all.x=T, all.y=F, sort=F)

      ## remaining NA's with cfdet are mostly due to bad hauls, broken nets etc.

      save (det, file=fn, compress=TRUE )
      return (fn)
    }


    # --------------------


    if (DS %in% c("cat", "cat.redo") ) {
      # all species caught
      cat = NULL # biologicals
      fn = file.path( surveydir, "cat.rdata"  )
      if (DS=="cat") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }

      set = survey.db( DS="set.init", p=p  ) # kg/km^2, no/km^2

      det = survey.db( DS="det", p=p  ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]

      cat = survey.db( DS="cat.init", p=p )
      cat = cat[ which( cat$id %in% unique( set$id) ), ]

      oo = which( duplicated( cat$id2) )
      if (length( oo) > 0 ) cat = cat[ -oo, ]


      cm = data.frame( id2=as.character( sort( unique( cat$id2 ) )), stringsAsFactors=FALSE )
      cm = merge( cm, applySum( det[ , c("id2", "mr", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )

      # averages of these variables
      newvars = c( "residual", "mass", "len", "Ea", "A", "Pr.Reaction", "smr"  )
      for ( nv in newvars ) {
      #browser()
        cm = merge( cm,
          applyMean( det[ , c("id2", nv, "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }

      cat = merge( cat, cm, by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )

      # where det measurements not available, estimate mean mass from total weights and numbers
      oo = which( !is.finite( cat$mass ))
      if (length(oo) > 0 ) {
        cat$mass[oo] = cat$totmass[oo] / cat$totno[oo]
      }

    	surveys = sort( unique( cat$data.source ) )
      species = sort( unique( cat$spec_bio ) )

			# in the following:	quantiles are computed,
      cat$qn = NA  # default when no data
      oo = which( cat$totno == 0 )  # retain as zero values
      if (length(oo)>0 ) cat$qn[oo] = 0

      for ( s in surveys ) {
        si = which( cat$data.source==s & cat$totno > 0 )
        for (sp in species ){
          spi = which( cat$spec_bio == sp )
          ii = intersect( si, spi )
          if (length( ii) > 0 ) {
						cat$qn[ii] = quantile_estimate( cat$totno[ii]  )  # convert to quantiles, by species and survey
					}
      }}

			cat$qm = NA   # default when no data
      oo = which( cat$totmass == 0 )  # retain as zero values
      if (length(oo)>0 ) cat$qm[oo] = 0

      for ( s in surveys ) {
        si = which( cat$data.source==s & cat$totmass > 0 )
        for (sp in species ){
          spi = which( cat$spec_bio == sp )
          ii = intersect( si, spi )
          if (length( ii) > 0 ) {
						cat$qm[ii] = quantile_estimate( cat$totmass[ii]  )  # convert to quantiles, by species and survey
					}
      }}

     # convert from quantile to z-score

      cat$zm = quantile_to_normal( cat$qm )
      cat$zn = quantile_to_normal( cat$qn )


			over.write.missing.data = TRUE
			if (over.write.missing.data) {

				# over-write na's for n or mass from each other, where possible:
				kxm = which( !is.finite( cat$qm) )
				kxn = which( !is.finite( cat$qn) )

				kmn = setdiff( kxm, kxn )
				knm = setdiff( kxn, kxm )

				if ( length( knm) > 0 ) cat$qn[knm] =  cat$qm[knm]
				if ( length( kmn) > 0 ) cat$qm[kmn] =  cat$qn[kmn]

				# remaining missing values take the median value for each species == 0.5
				kxm = which( !is.finite( cat$qm ) )
				if ( length( kxm) > 0 ) cat$qm[kxm] = 0.5

				kxn = which( !is.finite( cat$qn ) )
				if ( length( kxn) > 0 ) cat$qn[kxn] = 0.5

			}

      save (cat, file=fn, compress=TRUE )
      return (fn)

    }



    # -------------



    if (DS %in% c("set","set.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( surveydir, "set.rdata"  )
      if (DS=="set") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }

      set = survey.db( DS="set.intermediate", p=p )

      det = survey.db( DS="det", p=p  ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]

      cat = survey.db( DS="cat", p=p )
      cat = cat[ which( cat$id %in% unique( set$id) ), ]

      # NOTE: cat$totno and cat$totmass have already been cf corrected ---> already in per km2
      sm = data.frame( id=as.character( sort( unique( set$id ) )), stringsAsFactors=FALSE )

      # summaries from cat
      sm = merge( sm, applySum( cat[ , c("id", "totno") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applySum( cat[ , c("id", "totmass") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )



      # summaries from det
      # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years

      sm = merge( sm, applySum( det[ , c("id", "mr", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

      # averages of these variables from det
      newvars = c( "residual", "mass", "len", "Ea", "A", "Pr.Reaction", "smr"  )
      for ( nv in newvars ) {
        sm = merge( sm,
          applyMean( det[ , c("id", nv, "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }


      set = merge( set, sm, by ="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

    	surveys = sort( unique( set$data.source ) )

			# in the following:	quantiles are computed,
      set$qn = NA  # default when no data
      oo = which( set$totno == 0 )  # retain as zero values
      if (length(oo)>0 ) set$qn[oo] = 0

      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totno > 0 )
        if (length( ii) > 0 ) {
  				set$qn[ii] = quantile_estimate( set$totno[ii]  )  # convert to quantiles, by survey
				}
      }

			set$qm = NA   # default when no data
      oo = which( set$totmass == 0 )  # retain as zero values
      if (length(oo)>0 ) set$qm[oo] = 0

      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totmass > 0 )
          if (length( ii) > 0 ) {
						set$qm[ii] = quantile_estimate( set$totmass[ii]  )  # convert to quantiles, by survey
					}
      }

     # convert from quantile to z-score

      set$zm = quantile_to_normal( set$qm )
      set$zn = quantile_to_normal( set$qn )


			over.write.missing.data = TRUE
			if (over.write.missing.data) {

				# over-write na's for n or mass from each other, where possible:
				kxm = which( !is.finite( set$qm) )
				kxn = which( !is.finite( set$qn) )

				kmn = setdiff( kxm, kxn )
				knm = setdiff( kxn, kxm )

				if ( length( knm) > 0 ) set$qn[knm] =  set$qm[knm]
				if ( length( kmn) > 0 ) set$qm[kmn] =  set$qn[kmn]

				# remaining missing values take the median value == 0.5
				kxm = which( !is.finite( set$qm ) )
				if ( length( kxm) > 0 ) set$qm[kxm] = 0.5

				kxn = which( !is.finite( set$qn ) )
				if ( length( kxn) > 0 ) set$qn[kxn] = 0.5

			}


      save( set, file=fn, compress=T )
      return (fn)
    }


    # ---------------------


    if (DS == "set.filter" ) {

      # selected for a given set of species  and size and sex and maturity
      
      set = det = NULL # trip/set loc information
      
      set = survey.db( DS="set.intermediate", p=p )
      det = survey.db( DS="det", p=p  ) # size information, no, cm, kg

      det = det[ which( det$id %in% unique( set$id) ), ]

      isc = indicators_selection_criteria( det, selection=selection )
      det = det[isc,] 
     
      sm = data.frame( id=as.character( sort( unique( set$id ) )), stringsAsFactors=FALSE )

      # summaries from det
      # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years
      # cfdet is the weight to make it sum up to the correct total catch (vs any subsamples) and tow length, etc
      det$totno = 1
      sm = merge( sm, applySum( det[ , c("id", "totno", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
  
      det$totmass = det$mass
      sm = merge( sm, applySum( det[ , c("id", "totmass", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

      set = merge( set, sm, by ="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      set$totno[ which(!is.finite(set$totno))] = 0
      set$totmass[ which(!is.finite(set$totmass))] = 0

browser()

      if (exists("drop.groundfish.data", selection)) {
        # unreliable zero's for snowcrab in the groundfish data
        todrop = which( set$data.source=="groundfish" & set$yr < 1999 & (set$totmass ==0 | set$totno==0) )
        if (length(todrop)>0) set = set[-todrop,]
      }

      surveys = sort( unique( set$data.source ) )

      # in the following: quantiles are computed,
      set$qn = NA  # default when no data
      oo = which( set$totno == 0 )  # retain as zero values
      if (length(oo)>0 ) set$qn[oo] = 0

      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totno > 0 )
        if (length( ii) > 0 ) {
          set$qn[ii] = quantile_estimate( set$totno[ii]  )  # convert to quantiles, by survey
        }
      }

      set$qm = NA   # default when no data
      oo = which( set$totmass == 0 )  # retain as zero values
      if (length(oo)>0 ) set$qm[oo] = 0

      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totmass > 0 )
          if (length( ii) > 0 ) {
            set$qm[ii] = quantile_estimate( set$totmass[ii]  )  # convert to quantiles, by survey
          }
      }

     # convert from quantile to z-score
      set$zm = quantile_to_normal( set$qm )
      set$zn = quantile_to_normal( set$qn )

      return (set)
    }



  }


