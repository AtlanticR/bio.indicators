
 
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
    lbm( p=p, tasks=c( "stage1" ) ) #  ~1-2 hrs (1999-2016) 
    lbm( p=p, tasks=c( "stage2" ) ) #  ~ 1 hrs
    lbm( p=p, tasks=c( "save" ) )
    NULL

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
  
    p = make.list( list( yrs=p$yrs), Y=p )
    parallel.run( indicators.db, p=p, DS="predictions.redo" ) # warp predictions to other grids
    indicators.db( p=p, DS="lbm.stats.redo" ) # warp stats to other grids
    indicators.db( p=p, DS="complete.redo" )
    indicators.db( p=p, DS="baseline.redo" )
    indicators.map( p=p )

    if (0) {
      global_model = lbm_db( p=p, DS="global_model") 
      summary( global_model )
      plot(global_model)
    }
  }


# ----------------

ca1 :

Family: gaussian 
Link function: identity 

Formula:
ca1 ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.025523   0.004243   6.015 1.83e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       1.998      2  274.88  < 2e-16 ***
s(tsd.climatology)         1.976      2   77.31  < 2e-16 ***
s(tmean.climatology)       2.000      2 7016.86  < 2e-16 ***
s(log(t.range))            1.396      2   68.72  < 2e-16 ***
s(log(b.range))            1.969      2   21.13 4.59e-10 ***
s(log(z))                  1.983      2  112.45  < 2e-16 ***
s(log(dZ))                 1.888      2   14.47 1.98e-07 ***
s(log(ddZ))                1.856      2   12.15 2.05e-06 ***
s(log.substrate.grainsize) 1.752      2  274.56  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.642   Deviance explained = 64.2%
GCV = 0.37765  Scale est. = 0.37733   n = 20956
---


# -------------------
#  vn ="ca2"
 
Family: gaussian 
Link function: identity 

Formula:
ca2 ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.035122   0.003606   -9.74   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                               edf Ref.df        F  p-value    
s(t)                       1.91152      2  427.472  < 2e-16 ***
s(tsd.climatology)         1.99547      2  685.540  < 2e-16 ***
s(tmean.climatology)       2.00000      2  103.914  < 2e-16 ***
s(log(t.range))            1.99266      2   85.912  < 2e-16 ***
s(log(b.range))            1.59609      2   15.951 9.64e-09 ***
s(log(z))                  1.99890      2 4905.432  < 2e-16 ***
s(log(dZ))                 0.09956      2    0.051    0.311    
s(log(ddZ))                1.98690      2   31.422 7.97e-15 ***
s(log.substrate.grainsize) 1.99396      2  168.870  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.731   Deviance explained = 73.1%
GCV = 0.27272  Scale est. = 0.2725    n = 20956
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
