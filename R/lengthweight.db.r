
lengthweight.db = function( DS="update", x=NULL  ) {

  ## TODO -- make parallel require(multicore)

  ddir = file.path( project.datadirectory("bio.indicators", "data" ) )
  dir.create( ddir, showWarnings=FALSE, recursive=TRUE )

  fn = file.path( ddir, "bio.length.weight.parameters.rdata" )

  if (DS=="parameters") {
    res = NULL
    if (file.exists( fn ) ) load( fn )
    return( res )
  }

  if ( DS=="update" ) {

    if (is.null(x)) {
      # this mirrors the relevent changes/recoding in indicators.db("det")
      x = survey.db( DS="det.init" )
      x$spec = x$spec_bio
      x = x[ which( is.finite( x$spec)), ]
      x$sex[ which( !is.finite(x$sex)) ] = 2 # set all uncertain sexes to one code sex code
      x$mat[ which( !is.finite(x$mat)) ] = 2 # set all uncertain sexes to one code sex code
    }

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

      w = intersect( wsp, wqn )
      w = intersect( w, wsx )
      w = intersect( w, wmt )

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
    save( res, file=fn, compress=TRUE )
    return( fn )
  }

}


