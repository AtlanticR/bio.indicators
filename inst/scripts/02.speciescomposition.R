
 
  # -----------------------------
  # ordination
  current.year = 2016  # starting from 1999-present


  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )


  # -----------------------------
  # lbm; vn="ca1"
  for ( vn in p$varstomodel) {
    print(vn)

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    # lbm( p=p, tasks=c( "stage0" ) ) # serial mode
    # lbm( p=p, tasks=c( "continue" ) )    
    lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    lbm( p=p, tasks=c( "save" ) )
    

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
  
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db ( p=p, DS="complete.redo" )
    indicators.db ( p=p, DS="baseline.redo" )
    indicators.map( p=p )

    if (0) {
      global_model = lbm_db( p=p, DS="global_model") 
      summary( global_model )
      plot(global_model)
    }
  }


Family: gaussian 
Link function: identity 

Formula:
ca1 ~ s(t, bs = "ts") + s(tsd.climatology, bs = "ts") + s(tmean.climatology, 
    bs = "ts") + s(log(t.range), bs = "ts") + s(log(b.range), 
    bs = "ts") + s(log(s.range), bs = "ts") + s(log(z), bs = "ts") + 
    s(log(dZ), bs = "ts") + s(log(ddZ), bs = "ts") + s(log.substrate.grainsize, 
    bs = "ts")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.104507   0.005456   19.16   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       7.511      9  70.352  < 2e-16 ***
s(tsd.climatology)         7.102      9   4.182 9.33e-07 ***
s(tmean.climatology)       6.270      9 679.206  < 2e-16 ***
s(log(t.range))            5.772      9  11.599  < 2e-16 ***
s(log(b.range))            6.752      9   2.072 0.004846 ** 
s(log(s.range))            6.414      9   2.832 0.000149 ***
s(log(z))                  8.787      9  58.253  < 2e-16 ***
s(log(dZ))                 8.438      9   3.902 2.05e-05 ***
s(log(ddZ))                2.645      9   4.714 5.81e-11 ***
s(log.substrate.grainsize) 8.393      9   7.882 1.22e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.582   Deviance explained = 58.4%
GCV = 0.37548  Scale est. = 0.37341   n = 12545
---



# -------------------
#  vn ="ca2"
 
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
#  vn ="pca1"

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


# -------------------
#  vn ="pca2"
