
  indicators.db = function( ip=NULL, DS="baseline", p=NULL, year=NULL ) {

    # simple wrappers to load relevant, uninterpolated point data
    if (DS == "sizespectrum")  {
      o = bio.indicators::sizespectrum.db( DS=DS, p=p )
      return( o )
    }

    if (DS == "metabolism") {
      o = bio.indicators::metabolism.db( DS=DS, p=p ) 
      return(o)
    }   

    if (DS == "speciesarea") {
      o = bio.indicators::speciesarea.db( DS=DS, p=p ) 
      return( o)
    }  
    
    if (DS == "speciescomposition") {
      o = bio.indicators::speciescomposition.db( DS=DS, p=p )
      return( o )
    } 

    if (DS == "condition")  {
      o = bio.indicators::condition.db( DS=DS, p=p )
      return( o )
    }

    if (DS == "biochem") {
      bio.indicators::biochem.db( DS=DS, p=p )
      return( o )
    }  


    if (DS %in% c("baseline", "baseline.redo") ) {

      # form a basic prediction surface in planar coords for SS habitat for
      # factors that do not "change" rapidly and
      # NOTE this depends upon GMT ..  to be pahsed out .

      message( "This is deprecated due to GMT dependency .. kept for backwards compatability")

      outdir = file.path( project.datadirectory("bio.indicators", "habitat"), p$spatial.domain, "baseline" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.datadirectory("bio.indicators", "habitat"), "SSE", "baseline" )

      dir.create(outdir, recursive=T, showWarnings=F)
      outfile =  file.path( outdir, "PS.baseline.rdata" )

      if ( DS=="baseline" ) {
        if (file.exists(outfile)) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
        return (PS)
      }

			# depth is the primary constraint
      Zbase = bathymetry.db( p=p, DS="baseline" ) # area -prefiltered for depth/bounds
      Zbase$z = NULL
      Z = bathymetry.db( p=p, DS="complete" )  # SS to a depth of 500 m  the default used for all planar SS grids
      Z$id = 1:nrow(Z)
      Z$dZ = log( Z$dZ )
      Z$ddZ = log( Z$ddZ)
      Z = Z[, c("plon", "plat", "id", "z", "dZ", "ddZ" )]
      Z = merge(Zbase, Z, by=c("plon", "plat"), all.x=TRUE, all.y=FALSE, sort=FALSE )

      S =  substrate.db ( p=p, DS="planar")
      S$log.substrate.grainsize = log(S$grainsize)
      S$grainsize = NULL

      PS = merge( Z, S, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )

      print( "Interpolating missing data where possible.." )
        vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          print(v)

          for (dists in p$interpolation.distances) {
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0) break()
            print( paste("N = ", length(ii), "data points") )
            gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,],
                nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5))
            PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
        }}

      PS = PS[ order( PS$id), ]
      PS$id = NULL

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    #  -------------------------------


    if (DS %in% c("lbm_inputs", "lbm_inputs.redo") ) {

      if (p$project.name == "sizespectrum")  {
        IN = bio.indicators::sizespectrum.db( DS=p$project.name, p=p )
                        
      }

      if (p$project.name == "metabolisp$project.namem") {
        IN = bio.indicators::metabolism.db( DS=p$project.name, p=p ) 
    
      }   

      if (p$project.name == "speciesarea") {
        IN = bio.indicators::speciesarea.db( DS=p$project.name, p=p ) 

      }  
      
      if (p$project.name == "speciescomposition") {
        IN = bio.indicators::speciescomposition.db( DS=p$project.name, p=p )

      } 

      if (p$project.name == "condition")  {
        IN = bio.indicators::condition.db( DS=p$project.name, p=p )

      }

      if (p$project.name == "biochem") {
        IN = bio.indicators::biochem.db( DS=p$project.name, p=p )

      }  

      # truncate quantiles of dependent vars
      dr = list()
      for ( vm in p$varstomodel ) {
        dr[[vm]] = quantile( IN[,vm], probs=p$lbm_quantile_bounds, na.rm=TRUE ) # use 95%CI
        il = which( IN[,vm] < dr[[vm]][1] )
        if ( length(il) > 0 ) IN[il,vm] = dr[[vm]][1]
        iu = which( IN[,vm] > dr[[vm]][2] )
        if ( length(iu) > 0 ) IN[iu,vm] = dr[[vm]][2]
      }
    
      IN$z = log(IN$z)
      IN$dZ = log( IN$dZ )
      IN$ddZ = log( IN$ddZ)
      IN$log.substrate.grainsize = log(IN$grainsize)
      IN$tamp = log(IN$tamp)
   #   IN$tamp.cl = log(IN$tamp.cl)

      PS = indicators.db( p=p, DS="prediction.surface" ) 
      PS$z = log(PS$z)
      PS$dZ = log( PS$dZ )
      PS$ddZ = log( PS$ddZ)
      PS$log.substrate.grainsize = log(PS$grainsize)
      PS$tamp = log(PS$tamp)
   #   PS$tamp.cl = log(PS$tamp.cl)

      OUT = list( LOCS=PS[, c("plon","plat")], COV =as.list( PS[, p$lbm_covars]) )          

      return (list(input=IN, output=OUT))

    }



    #  -------------------------------


    if (DS %in%  c("prediction.surface", "prediction.surface.redo") ) {
      fn = file.path( project.datadirectory("bio.indicators")0, "analysis", "habitat", p$spatial.domain, "prediction.surface.rdata" )
      PS = NULL
      
      if (DS=="prediction.surface") {
        if (file.exists(fn)) load(fn)
        return(PS)      
      }

      # default output grid
      Z = bathymetry.db( p=p, DS="complete" )  # SS to a depth of 500 m  the default used for all planar SS grids
      Z = Z[ which(Z$z >0), ] 
      zid = lbm::array_map( "2->1", trunc(cbind(Z$plon-p$plon[1], Z$plat-p$plat[1])/p$pres) + 1, c(p$nplons,p$nplats) )
      
      Z$id = 1:nrow(Z)
      S = substrate.db( DS="complete" ) 
      sid = lbm::array_map( "2->1", trunc(cbind(S$plon-p$plon[1], S$plat-p$plat[1])/p$pres) + 1, c(p$nplons,p$nplats) )
      u = match( sid, zid )
      
      PS = cbind(S, B[u, ] )
      PS = PS[ is.finite( rowSums(PS) ), ]

      # add climatology temperatures
      TM = temperature.db( p=p, DS="climatology" )
      tid = lbm::array_map( "2->1", trunc(cbind(TM$plon-p$plon[1], TM$plat-p$plat[1])/p$pres) + 1, c(p$nplons,p$nplats) )
     
      # choose temps at p$prediction.dyear 
      v = match( sid, zid )
      
      PS = merge( PS, TM, by="id")

      TM = temperature.db( p=p, DS="timeslice", dyear= )


      # print( "Interpolating missing data where possible.." )
      #   vars = setdiff( names(PS), c("plon", "plat") )
      #   require (gstat)
      #   for (v in vars) {
      #     print(v)

      #     for (dists in p$interpolation.distances) {
      #       ii = which ( !is.finite( PS[, v]) )
      #       if (length(ii)==0) break()
      #       print( paste("N = ", length(ii), "data points") )
      #       gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,],
      #           nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5))
      #       PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
      #   }}


      PS = PS[ order( PS$id), ]
      save(PS, file=fn, compress=TRUE )
      return(fn)
  
    }


    #  -------------------------------


    if (DS %in% c("environmentals", "environmentals.redo") ) {

### NOTE -- "complete", "complete.redo" here as a temporay measure to skip other indicators for 2015/2016

      outdir =  file.path( project.datadirectory("bio.indicators", "analysis", "habitat"), p$spatial.domain, "environmentals" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.datadirectory("bio.indicators", "analysis", "habitat"), "SSE","environmentals" )
      dir.create(outdir, recursive=T, showWarnings=F)

      if ( DS %in% c( "environmentals")  ) {
        outfile =  file.path( outdir, paste( "PS", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
        return (PS)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if (!exists("ip")) ip = 1:p$nruns

      for (iy in ip) {
        yr = p$runs[iy, "yrs"]
        print(yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
        PS = NULL
        PS = indicators.db( DS="baseline", p=p )
        PS$id = 1:nrow(PS)
        E = temperature.db( DS="complete", p=p, year=yr  )
        E$z = NULL
        PS = merge( PS, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F)
        PS = PS[ order( PS$id ) ,]
        PS$id = NULL
        save (PS, file=outfile, compress=T )
        print( outfile)
      }
    }


    #  -------------------------------



    if (DS %in% c("complete", "complete.redo") ) {

      outdir =  file.path( project.datadirectory("bio.indicators", "analysis", "habitat"), p$spatial.domain, "complete" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.datadirectory("bio.indicators", "analysis", "habitat"), "SSE","complete" )
      dir.create(outdir, recursive=T, showWarnings=F)

      if ( DS=="complete" ) {
        outfile =  file.path( outdir, paste( "PS", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }

        return (PS)
      }

      if (exists( "libs", p)) RLibrary( p$libs )
      if (is.null(ip)) ip = 1:p$nruns

      for (iy in ip) {
        yr = p$runs[iy, "yrs"]
        print (yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )

        PS = indicators.db( DS="environmentals", p=p, year=yr )
        PS$id = 1:nrow(PS)

        # ---------------------
        # Species-area
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = indicators.parameters( "speciesarea", p=p )
        SAG =  habitat.interpolate( DS="all", p=pm, yr=max(1970,yr)   )
        # remove duplicates derived from repeated tows
        oo = which( duplicated (SAG$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SAG$platplon == SAG$platplon[o] )
            for (w in pm$speciesarea.variables ) {
              SAG[i[1],w] = mean(SAG[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SAG = SAG[ -todrop, ]
        }
        SAG = SAG[ , c("plon", "plat", pm$speciesarea.variables ) ]
        PS = merge( PS, SAG, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sag" ) )
        rm (SAG)


        # ---------------------
        # Species composition
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = indicators.parameters( "speciescomposition", p=p )
        SC = habitat.interpolate( DS="all", p=pm, yr=max(1970,yr) )

        # remove duplicates derived from repeated tows --- slow ...
        oo = which( duplicated (SC$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SC$platplon == SC$platplon[o] )
            for (w in pm$speciescomposition.variables ) {
              SC[i[1],w] = mean(SC[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SC = SC[ -todrop, ]
        }
        SC = SC[ , c("plon", "plat", pm$speciescomposition.variables ) ]
        PS = merge( PS, SC, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sc" ) )
        rm (SC)


        # ---------------------
        # size spectrum stats
        pm = indicators.parameters( "sizespectrum", p=p )
        SS = habitat.interpolate ( DS="all", p=pm, yr=max(1970,yr) )

        # remove duplicates derived from repeated tows --- slow ...
        oo = which( duplicated (SS$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SS$platplon == SS$platplon[o] )
            for (w in pm$sizespectrum.variables) {
              SS[i[1],w] = mean(SS[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SS = SS[ -todrop, ]
        }
        SS = SS[ , c("plon", "plat", pm$sizespectrum.variables ) ]
        PS = merge( PS, SS, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".ss" ) )
        rm(SS)


       # ---------------
       # Condition db
       pm = indicators.parameters( "condition", p=p )
       CD = habitat.interpolate ( DS="all", p=pm,  yr=max(1970,yr) )
       # remove duplicates derived from repeated tows --- slow ...
       oo = which( duplicated (CD$platplon ) )
       if (length( oo)> 0 ) {
         todrop= NULL
         for (o in oo ) {
           i = which( CD$platplon == CD$platplon[o] )
           for (w in pm$condition.variables) {
             CD[i[1],w] = mean(CD[i,w], na.rm=TRUE)
             todrop = c(todrop, i[-1])
           }
         }
         CD = CD[ -todrop, ]
       }
       CD = CD[ , c("plon", "plat", pm$condition.variables ) ]
       PS = merge( PS, CD, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".CD" ) )
       rm(CD)

        # ---------------
        # metabolic rates
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = indicators.parameters( "metabolism", p=p )
        MR = habitat.interpolate ( DS="all", p=pm, yr=max(1970,yr) )
        # remove duplicates derived from repeated tows --- slow ...
        oo = which( duplicated (MR$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( MR$platplon == MR$platplon[o] )
            for (w in pm$metabolism.variables) {
              MR[i[1],w] = mean(MR[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          MR = MR[ -todrop, ]
        }
        MR = MR[ , c("plon", "plat", pm$metabolism.variables ) ]
        PS = merge( PS, MR, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".mr" ) )
        rm(MR)

        vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          for (dists in p$interpolation.distances) {
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0 | length(ii)==nrow(PS) ) break()
            print( paste( "Interpolating missing data with inverse-distance weighted means", v, ":", length(ii) ) )
            gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,],
                nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5))
            PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
        }}

        PS = PS[ order( PS$id ) ,]
        PS$id =NULL
        PS$Y = 1 # required to run "model.matrix"
        if (yr < min( p$yearstomodel ) ) PS$yr = max( p$yearstomodel )  # most recent year as reference

        for (o in 1:ncol(PS) ) attributes( PS[,o]) <- NULL  # remove rownames, etc .. reduces size of data object

        save (PS, file=outfile, compress=T )
      }
      return( "Complete" )
    }




  }


