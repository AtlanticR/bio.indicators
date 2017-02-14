
 
  # -----------------------------
  # ordination
  current.year = 2016  # starting from 1999-present


  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
  bio.indicators::speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  bio.indicators::speciescomposition.db( DS="speciescomposition.redo", p=p )


  # -----------------------------
  # lbm; vn="pca2"
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
  
    # p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    # p = bio.indicators::indicators.parameters( p=p, DS="speciescomposition"  )
    # p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
  
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
pca1 ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0584274  0.0007373   79.25   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df        F  p-value    
s(t)                       1.9999      2  360.999  < 2e-16 ***
s(tsd.climatology)         1.9883      2  127.604  < 2e-16 ***
s(tmean.climatology)       1.9974      2 7192.129  < 2e-16 ***
s(log(t.range))            1.8631      2   82.989  < 2e-16 ***
s(log(b.range))            1.9895      2   26.450  2.6e-12 ***
s(log(z))                  1.9881      2  121.207  < 2e-16 ***
s(log(dZ))                 0.9554      2    4.122 0.001560 ** 
s(log(ddZ))                1.1569      2    6.504 0.000124 ***
s(log.substrate.grainsize) 1.9812      2  400.642  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.664   Deviance explained = 66.5%
GCV = 0.011401  Scale est. = 0.011392  n = 20956


# -------------------
#  vn ="pca2"
Family: gaussian 
Link function: identity 

Formula:
pca2 ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0106285  0.0006277   16.93   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                 edf Ref.df        F  p-value    
s(t)                       1.971e+00      2   95.780  < 2e-16 ***
s(tsd.climatology)         1.999e+00      2  426.149  < 2e-16 ***
s(tmean.climatology)       1.992e+00      2  134.237  < 2e-16 ***
s(log(t.range))            1.957e+00      2   18.671 5.24e-09 ***
s(log(b.range))            1.973e+00      2   45.553  < 2e-16 ***
s(log(z))                  1.973e+00      2 2782.931  < 2e-16 ***
s(log(dZ))                 1.041e-07      2    0.000 0.648521    
s(log(ddZ))                1.940e+00      2    8.724 0.000125 ***
s(log.substrate.grainsize) 1.811e+00      2  516.626  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.676   Deviance explained = 67.6%
GCV = 0.0082627  Scale est. = 0.0082561  n = 20956
---
