
      indicators.lookup = function( p, DS, locsmap=NULL, locs=NULL, timestamp=NULL, varnames=NULL, DB=NULL ) {

        if (is.null(locsmap)){
          gridparams = list( dims=c(p$nplons, p$nplats), corner=c(p$plons[1], p$plats[1]), res=c(p$pres, p$pres) )
          grid = lbm::array_map( "xy->1", locs, gridparams=gridparams )
          baid = lbm::array_map( "xy->1", bathymetry.db(p=p, DS="baseline"), gridparams=gridparams )
          locsmap = match( grid, baid )
        }

        if (DS=="spatial"){
          if (is.null(DB)) DB = indicators.db(p=p, DS="spatial")
          if (is.null(varnames)) varnames=names(DB)
          out = DB[locsmap,varnames]
          return(out)
        }

        if (DS=="spatial.annual"){ 
          out = NULL
          dindex = cbind(locsmap, match( lubridate::years(timestamp), p$yrs ) )
          if (is.null(DB)) DB = indicators.db(p=p, DS="spatial.annual")
          if (is.null(varnames)) varnames=names(DB)
          for (vn in varnames){
            out = cbind( out, DB[[vn]][dindex] )
          }
          return(out)
        }

        if (DS=="spatial.annual.seasonal"){ 
          out = NULL
          yrs = lubridate::year(timestamp)
          dyear = lubridate::decimal_date( timestamp ) -yrs
          dyear_index = as.numeric( cut( dyear, breaks=p$dyears, include.lowest=T, ordered_result=TRUE ) )
          dindex = cbind(locsmap, match( yrs, p$yrs ), dyear_index ) # check this
          if (is.null(DB)) {
            if (!is.null(varnames)) DB=indicators.db(p=p, DS=varnames) # at this point this is the only database with seasonality .. other stats (than mean) will require supplemntary functionss
          }
          out = DB[dindex] )
          return(out)
        }


      }
