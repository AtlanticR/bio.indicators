
      indicators.lookup = function( p, DS, locsmap=NULL, locs=NULL, timestamp=NULL, varnames=NULL, DB=NULL ) {

        if (is.null(locsmap)){
          grid = lbm::array_map( "xy->1", locs, gridparams=p$gridparams )
          baid = lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=p$gridparams )
          locsmap = match( grid, baid )
        }

        if (DS=="spatial"){
          if (is.null(DB)) DB = indicators.db(p=p, DS="spatial")
          if (is.null(varnames)) varnames=names(DB)
          vnames_DB = names(DB)
          varnames = intersect( vnames_DB, varnames )
          out = DB[locsmap,varnames]
          return(out)
        }

        if (DS=="spatial.annual"){ 
          out = NULL
          dindex = cbind(locsmap, match( lubridate::years(timestamp), p$yrs ) )
          if (is.null(DB)) DB = indicators.db(p=p, DS="spatial.annual")
          if (is.null(varnames)) varnames=names(DB)
          vnames_DB = names(DB)
          varnames = intersect( vnames_DB, varnames )
          for (vn in varnames){
            out = cbind( out, DB[[vn]][dindex] )
          }
          names(out) = c( vnames_DB, varnames )
          return(out)
        }

        if (DS=="spatial.annual.seasonal"){ 
          # only temp for now
          out = NULL
          yrs = lubridate::year(timestamp)
          dyear = lubridate::decimal_date( timestamp ) -yrs
          dyear_index = as.numeric( cut( dyear, breaks=p$dyears, include.lowest=T, ordered_result=TRUE ) )
          dindex = cbind(locsmap, match( yrs, p$yrs ), dyear_index ) # check this
          if (is.null(DB)) {
            if (!is.null(varnames)) DB=indicators.db(p=p, DS=varnames) # at this point this is the only database with seasonality .. other stats (than mean) will require supplemntary functionss
          }
          if (is.null(varnames)) varnames=names(DB)
          vnames_DB = names(DB)
          varnames = intersect( vnames_DB, varnames )
          out = DB[dindex] 
          return(out)
        }


        if (DS=="baseline"){ 
          # all interpolated fields
          out = NULL
          dindex = cbind(locsmap, match( lubridate::years(timestamp), p$yrs ) )
          DB = indicators.db(p=p, DS="baseline")
          if (is.null(varnames)) varnames=names(DB)
          vnames_DB = names(DB)
          varnames = intersect( vnames_DB, varnames )
          for (vn in varnames){
            out = cbind( out, DB[[vn]][dindex] )
          }
          
          return(out)
        }


      }
