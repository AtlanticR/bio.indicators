
 
  # -----------------------------
  # ordination

  # vn ="ca1"
  
  current.year = 2016
  
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )

  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )
  for ( vn in p$varstomodel) {
    print(vn)
    
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    p = lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   p = lbm( p=p, tasks=c( "stage0" ) ) # serial mode
#   p = lbm( p=p, tasks=c( "continue" ) )    
    p = lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    p = lbm( p=p, tasks=c( "stage2" ) ) #   1 hrs
    p = lbm( p=p, tasks=c( "save" ) )
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db ( p=p, DS="complete.redo" )
    indicators.db ( p=p, DS="baseline.redo" )
    
    indicators.map( p=p, DS="all")

  }



   p$variable$Y="ca2"
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




# -------------------

   p$variable$Y="ca2"
   global_model = lbm_db( p=p, DS="global_model") 
   summary( global_model )
   plot(global_model)


Family: gaussian 
Link function: identity 

Formula:
ca2 ~ s(yr) + s(dyear, k = 3, bs = "ts") + s(yr, dyear, k = 36, 
    bs = "ts") + s(t, bs = "ts") + s(tmean, bs = "ts") + s(tamplitude, 
    bs = "ts") + s(z, bs = "ts") + s(dZ, bs = "ts") + s(ddZ, 
    bs = "ts") + s(log.substrate.grainsize, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.035536   0.002986   -11.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df        F  p-value    
s(yr)                       1.529  1.886    1.623    0.154    
s(dyear)                    1.994  2.000 1564.091  < 2e-16 ***
s(yr,dyear)                32.999 33.000    6.903  < 2e-16 ***
s(t)                        8.469  9.000   81.878  < 2e-16 ***
s(tmean)                    8.284  9.000   41.237  < 2e-16 ***
s(tamplitude)               5.023  9.000   43.472  < 2e-16 ***
s(z)                        8.860  9.000 3456.474  < 2e-16 ***
s(dZ)                       6.892  9.000    4.974 1.94e-08 ***
s(ddZ)                      3.144  9.000    4.577 5.18e-10 ***
s(log.substrate.grainsize)  8.178  9.000   59.988  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.816   Deviance explained = 81.6%
GCV = 0.18785  Scale est. = 0.18707   n = 20979
---




