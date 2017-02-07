
  # -----------------------------
  # estimate metabolic demand, given size structure
  

  current.year = 2016
  
  # vn="smr"
  

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
 
  p = bio.indicators::indicators.parameters( p=p, DS="metabolism")
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )
# o = bio.indicators::metabolism.db( DS="metabolism", p=p ) 

  p0 = p # lbm overwrites p .. store the parent copy
  for ( vn in p$varstomodel) {
    print(vn)
    
    p = bio.indicators::indicators.parameters( p=p0, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    p = lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   p = lbm( p=p, tasks=c( "stage0" ) ) # serial mode
#   p = lbm( p=p, tasks=c( "continue" ) )    
    p = lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    p = lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    p = lbm( p=p, tasks=c( "save" ) )
    
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db ( p=p, DS="complete.redo" )
    indicators.db ( p=p, DS="baseline.redo" )
    indicators.map( p=p )
  }

   vn = "smr"
   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
   global_model = lbm_db( p=p, DS="global_model") 
   summary( global_model )
   plot(global_model)


Family: gaussian 
Link function: identity 

Formula:
smr ~ s(yr) + s(dyear, k = 3, bs = "ts") + s(yr, dyear, k = 36, 
    bs = "ts") + s(t, bs = "ts") + s(tmean, bs = "ts") + s(tamplitude, 
    bs = "ts") + s(z, bs = "ts") + s(dZ, bs = "ts") + s(ddZ, 
    bs = "ts") + s(log.substrate.grainsize, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.240e-03  1.036e-05   505.6   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df       F  p-value    
s(yr)                       1.000      1   1.647 0.199440    
s(dyear)                    1.968      2  17.592 1.35e-08 ***
s(yr,dyear)                31.775     33   9.118  < 2e-16 ***
s(t)                        8.838      9 243.975  < 2e-16 ***
s(tmean)                    4.691      9  19.329  < 2e-16 ***
s(tamplitude)               2.816      9   7.297 2.36e-16 ***
s(z)                        8.732      9  26.166  < 2e-16 ***
s(dZ)                       5.003      9   2.319 0.000339 ***
s(ddZ)                      6.940      9   5.754 6.46e-10 ***
s(log.substrate.grainsize)  7.621      9  37.354  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.207   Deviance explained =   21%
GCV = 2.3543e-06  Scale est. = 2.3456e-06  n = 21839
---
