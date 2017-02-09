
 
  # -----------------------------
  # ordination

  # vn ="ca1"
  
  current.year = 2016
  
  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
  
  p = make.list( list( yrs=p$yrs), Y=p )
  p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )
 
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )
  
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


   vn = "ca1"
   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
   global_model = lbm_db( p=p, DS="global_model") 
   summary( global_model )
   plot(global_model)

Family: gaussian 
Link function: identity 

Formula:
ca1 ~ s(t, bs = "ts") + s(tmean, bs = "ts") + s(tamplitude, bs = "ts") + 
    s(z, bs = "ts") + s(dZ, bs = "ts") + s(ddZ, bs = "ts") + 
    s(log.substrate.grainsize, bs = "ts")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.026685   0.004749   5.619 1.94e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       8.595      9 124.208  < 2e-16 ***
s(tmean)                   8.473      9 573.784  < 2e-16 ***
s(tamplitude)              3.058      9  20.523  < 2e-16 ***
s(z)                       7.738      9  81.616  < 2e-16 ***
s(dZ)                      7.466      9   5.525 4.92e-09 ***
s(ddZ)                     8.616      9  20.360  < 2e-16 ***
s(log.substrate.grainsize) 7.759      9  81.360  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.551   Deviance explained = 55.3%
GCV = 0.47429  Scale est. = 0.4731    n = 20979
---




# -------------------

   vn = "ca2"
   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
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





# -------------------

   vn = "pca1"
   p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
   global_model = lbm_db( p=p, DS="global_model") 
   summary( global_model )
   plot(global_model)


Family: gaussian 
Link function: identity 

Formula:
pca1 ~ s(yr) + s(dyear, k = 3, bs = "ts") + s(yr, dyear, k = 36, 
    bs = "ts") + s(t, bs = "ts") + s(tmean, bs = "ts") + s(tamplitude, 
    bs = "ts") + s(z, bs = "ts") + s(dZ, bs = "ts") + s(ddZ, 
    bs = "ts") + s(log.substrate.grainsize, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0582201  0.0006144   94.75   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df       F  p-value    
s(yr)                       4.787  4.898   0.523   0.7346    
s(dyear)                    1.987  2.000 574.538  < 2e-16 ***
s(yr,dyear)                28.648 33.000  41.190  < 2e-16 ***
s(t)                        8.631  9.000 210.376  < 2e-16 ***
s(tmean)                    8.393  9.000 418.908  < 2e-16 ***
s(tamplitude)               3.068  9.000   7.410 2.28e-16 ***
s(z)                        8.719  9.000 155.098  < 2e-16 ***
s(dZ)                       6.647  9.000   1.854   0.0106 *  
s(ddZ)                      7.265  9.000   7.519 3.78e-13 ***
s(log.substrate.grainsize)  8.345  9.000  62.564  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.767   Deviance explained = 76.8%
GCV = 0.0079537  Scale est. = 0.0079206  n = 20979
---
