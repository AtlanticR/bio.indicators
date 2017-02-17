
  # -----------------------------
  # estimate metabolic demand, given size structure
  

  current.year = 2016
  

  p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
  p = bio.indicators::indicators.parameters( p=p, DS="metabolism")
  bio.indicators::metabolism.db( DS="metabolism.redo", p=p )
# o = bio.indicators::metabolism.db( DS="metabolism", p=p ) 


 # -----------------------------
  # lbm; vn="smr"
  for ( vn in p$varstomodel) {
    print(vn)

    p = bio.indicators::indicators.parameters( DS="default", current.year=current.year )
    p = bio.indicators::indicators.parameters( p=p, DS="metabolism"  )
    p = bio.indicators::indicators.parameters( p=p, DS="lbm", varname=vn )
    DATA='indicators.db( p=p, DS="lbm_inputs" )'
    lbm( p=p, DATA=DATA, tasks=c("initiate", "globalmodel") ) # 5 min
    #   lbm( p=p, tasks=c( "stage0" ) ) # serial mode
    #   lbm( p=p, tasks=c( "continue" ) )    
    lbm( p=p, tasks=c( "stage1" ) ) #  8 hrs 
    lbm( p=p, tasks=c( "stage2" ) ) #  1 hrs
    lbm( p=p, tasks=c( "save" ) )
 
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



---
Family: gaussian 
Link function: identity 

Formula:
mr ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  15253.7      202.8   75.23   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                              edf Ref.df       F  p-value    
s(t)                       1.5477      2 129.275  < 2e-16 ***
s(tsd.climatology)         1.9837      2  36.564  < 2e-16 ***
s(tmean.climatology)       1.8999      2  10.677 9.44e-06 ***
s(log(t.range))            1.9377      2  15.969 6.41e-08 ***
s(log(b.range))            1.4403      2   2.834 0.024740 *  
s(log(z))                  0.9176      2   4.735 0.000536 ***
s(log(dZ))                 1.8186      2  13.770 2.17e-07 ***
s(log(ddZ))                1.7520      2   2.385 0.064152 .  
s(log.substrate.grainsize) 1.8101      2  42.411  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0494   Deviance explained = 5.01%
GCV = 9.1465e+08  Scale est. = 9.1399e+08  n = 22230
---


Family: gaussian 
Link function: identity 

Formula:
smr ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0055192  0.0000149   370.3   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       1.983      2 586.368  < 2e-16 ***
s(tsd.climatology)         1.998      2  74.818  < 2e-16 ***
s(tmean.climatology)       1.463      2  76.785  < 2e-16 ***
s(log(t.range))            1.800      2   5.143   0.0034 ** 
s(log(b.range))            1.999      2  22.493 1.67e-10 ***
s(log(z))                  1.973      2  81.062  < 2e-16 ***
s(log(dZ))                 1.785      2   1.585   0.1684    
s(log(ddZ))                1.976      2  27.487 5.79e-13 ***
s(log.substrate.grainsize) 1.976      2  34.289 7.35e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.125   Deviance explained = 12.6%
GCV = 2.8606e-06  Scale est. = 2.8566e-06  n = 12860




Family: gaussian 
Link function: identity 

Formula:
Pr.Reaction ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, 
    bs = "ts") + s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.3178636  0.0001544    8534   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df         F  p-value    
s(t)                       2.000      2 4.116e+05  < 2e-16 ***
s(tsd.climatology)         1.999      2 7.274e+01  < 2e-16 ***
s(tmean.climatology)       1.949      2 9.606e+01  < 2e-16 ***
s(log(t.range))            1.993      2 2.489e+01 1.39e-11 ***
s(log(b.range))            1.875      2 4.128e+00  0.01224 *  
s(log(z))                  1.995      2 7.228e+01  < 2e-16 ***
s(log(dZ))                 1.469      2 9.550e-01  0.25856    
s(log(ddZ))                1.861      2 5.331e+00  0.00287 ** 
s(log.substrate.grainsize) 1.834      2 3.186e+00  0.03113 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.986   Deviance explained = 98.6%
GCV = 0.00055543  Scale est. = 0.000555  n = 23274
---






Family: gaussian 
Link function: identity 

Formula:
Ea ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -616.7698     0.1834   -3363   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df         F  p-value    
s(t)                       2.000      2 8.236e+05  < 2e-16 ***
s(tsd.climatology)         1.997      2 7.086e+01  < 2e-16 ***
s(tmean.climatology)       1.915      2 9.120e+01  < 2e-16 ***
s(log(t.range))            1.979      2 1.915e+01 3.82e-09 ***
s(log(b.range))            1.656      2 1.469e+00  0.16791    
s(log(z))                  1.978      2 5.256e+01  < 2e-16 ***
s(log(dZ))                 1.591      2 1.452e+00  0.15461    
s(log(ddZ))                1.726      2 4.613e+00  0.00471 ** 
s(log.substrate.grainsize) 1.827      2 2.821e+00  0.04527 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.994   Deviance explained = 99.4%
GCV = 783.38  Scale est. = 782.78    n = 23274
---




Family: gaussian 
Link function: identity 

Formula:
A ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 4.022e-03  8.277e-06   485.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       1.845      2  20.925 1.73e-10 ***
s(tsd.climatology)         1.921      2  43.541  < 2e-16 ***
s(tmean.climatology)       1.981      2 223.441  < 2e-16 ***
s(log(t.range))            1.803      2   7.098 0.000413 ***
s(log(b.range))            1.999      2  14.331 5.82e-07 ***
s(log(z))                  1.974      2  42.884  < 2e-16 ***
s(log(dZ))                 1.789      2   2.620 0.053728 .  
s(log(ddZ))                1.975      2  19.701 1.61e-09 ***
s(log.substrate.grainsize) 1.901      2  93.460  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0999   Deviance explained = 10.1%
GCV = 1.5244e-06  Scale est. = 1.5231e-06  n = 22230
---




Family: gaussian 
Link function: identity 

Formula:
zm ~ s(t, k = 3, bs = "ts") + s(tsd.climatology, k = 3, bs = "ts") + 
    s(tmean.climatology, k = 3, bs = "ts") + s(log(t.range), 
    k = 3, bs = "ts") + s(log(b.range), k = 3, bs = "ts") + s(log(z), 
    k = 3, bs = "ts") + s(log(dZ), k = 3, bs = "ts") + s(log(ddZ), 
    k = 3, bs = "ts") + s(log.substrate.grainsize, k = 3, bs = "ts")

Parametric coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.4998119  0.0009438   529.6   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                             edf Ref.df       F  p-value    
s(t)                       1.990      2 388.578  < 2e-16 ***
s(tsd.climatology)         1.987      2 118.949  < 2e-16 ***
s(tmean.climatology)       1.954      2  52.391  < 2e-16 ***
s(log(t.range))            1.975      2  41.094  < 2e-16 ***
s(log(b.range))            1.956      2  47.027  < 2e-16 ***
s(log(z))                  1.945      2 146.267  < 2e-16 ***
s(log(dZ))                 1.843      2   5.737 0.001912 ** 
s(log(ddZ))                1.863      2   6.990 0.000527 ***
s(log.substrate.grainsize) 1.922      2  58.181  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.103   Deviance explained = 10.4%
GCV = 0.020771  Scale est. = 0.020755  n = 23302
---



