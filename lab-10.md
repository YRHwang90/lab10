Lab 10 - Grading the professor, Pt. 2
================
Insert your name here
Insert date here

### Load packages and data

``` r
library(tidyverse) 
```

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

``` r
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.1.3

    ## Warning: package 'broom' was built under R version 4.1.3

    ## Warning: package 'parsnip' was built under R version 4.1.3

    ## Warning: package 'recipes' was built under R version 4.1.3

    ## Warning: package 'tune' was built under R version 4.1.3

    ## Warning: package 'workflows' was built under R version 4.1.3

    ## Warning: package 'workflowsets' was built under R version 4.1.3

``` r
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.1.3

    ## Warning: package 'airports' was built under R version 4.1.3

    ## Warning: package 'cherryblossom' was built under R version 4.1.3

    ## Warning: package 'usdata' was built under R version 4.1.3

``` r
library(openintro)
```

### Exercise 1

``` r
evals2<-evals

rr<-linear_reg(mode="regression") %>%
  set_engine("lm")

m_bty<-rr %>%
  fit(score~bty_avg, data=evals2)
  
m_bty
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg  
    ##     3.88034      0.06664

``` r
summary(m_bty$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

Multiple R-squared: 0.03502, Adjusted R-squared: 0.03293
y=3.88+0.07bty_avg

### Exercise 2

``` r
rr<-linear_reg(mode="regression") %>%
  set_engine("lm")

m_bty_gen<-rr %>%
  fit(score~bty_avg+factor(gender), data=evals2)
  
m_bty_gen
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + factor(gender), data = data)
    ## 
    ## Coefficients:
    ##        (Intercept)             bty_avg  factor(gender)male  
    ##            3.74734             0.07416             0.17239

``` r
summary(m_bty_gen$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + factor(gender), data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg             0.07416    0.01625   4.563 6.48e-06 ***
    ## factor(gender)male  0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

Multiple R-squared: 0.05912, Adjusted R-squared: 0.05503
y=3.74+0.07*bty_evg + 0.17*gender 5% male y=3.74+0.17+0.07\*bty_evg male
being a male is beneficial to get a high score, when the beaty score is
the same.

8 adding gender variable is beneficial for get a higher R^2

9 not that much

##ex10

``` r
rr<-linear_reg(mode="regression") %>%
  set_engine("lm")

m_bty_rank<-rr %>%
  fit(score~bty_avg+rank, data=evals2)
  
m_bty_rank
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + rank, data = data)
    ## 
    ## Coefficients:
    ##      (Intercept)           bty_avg  ranktenure track       ranktenured  
    ##          3.98155           0.06783          -0.16070          -0.12623

``` r
summary(m_bty_rank$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg + rank, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

y = 3.98 + 0.06bty_avg -0.16 rank(tenure track) - 0.12rank(tenured)

compared to the reference group, being a tenure tract or tenured is
detrimental to y

##ex11,12

``` r
ci <- lm(score ~ cls_profs, data=evals2)
summary(ci)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_profs, data = evals2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8554 -0.3846  0.1154  0.4154  0.8446 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.18464    0.03111 134.493   <2e-16 ***
    ## cls_profssingle -0.02923    0.05343  -0.547    0.585    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5443 on 461 degrees of freedom
    ## Multiple R-squared:  0.0006486,  Adjusted R-squared:  -0.001519 
    ## F-statistic: 0.2992 on 1 and 461 DF,  p-value: 0.5847

p=0.58… cls_profs is not significant predictor to score.

13 cls_did eval may overlapped with other predictors.

##ex1415

``` r
ex14<- rr %>%
  fit(score~rank+ethnicity+gender+language+age+
        cls_perc_eval+cls_students+cls_level + cls_profs+
        cls_credits+bty_avg, data=evals2 )
summary(ex14$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + ethnicity + gender + language + 
    ##     age + cls_perc_eval + cls_students + cls_level + cls_profs + 
    ##     cls_credits + bty_avg, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
back<-lm(score~factor(rank)+factor(ethnicity)+factor(gender)+factor(language)+age+
        cls_perc_eval+cls_students+factor(cls_level) + factor(cls_profs)+
        factor(cls_credits)+bty_avg, data=evals2 )

summary(back)
```

    ## 
    ## Call:
    ## lm(formula = score ~ factor(rank) + factor(ethnicity) + factor(gender) + 
    ##     factor(language) + age + cls_perc_eval + cls_students + factor(cls_level) + 
    ##     factor(cls_profs) + factor(cls_credits) + bty_avg, data = evals2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    3.5305036  0.2408200  14.660  < 2e-16 ***
    ## factor(rank)tenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## factor(rank)tenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## factor(ethnicity)not minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## factor(gender)male             0.1786166  0.0515346   3.466 0.000579 ***
    ## factor(language)non-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                           -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval                  0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students                   0.0004455  0.0003585   1.243 0.214596    
    ## factor(cls_level)upper         0.0187105  0.0555833   0.337 0.736560    
    ## factor(cls_profs)single       -0.0085751  0.0513527  -0.167 0.867458    
    ## factor(cls_credits)one credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                        0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
back2<-stats::step(back, direction="backward")
```

    ## Start:  AIC=-621.66
    ## score ~ factor(rank) + factor(ethnicity) + factor(gender) + factor(language) + 
    ##     age + cls_perc_eval + cls_students + factor(cls_level) + 
    ##     factor(cls_profs) + factor(cls_credits) + bty_avg
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## - factor(rank)         2    0.4325 114.74 -623.91
    ## - factor(cls_profs)    1    0.0071 114.31 -623.63
    ## - factor(cls_level)    1    0.0288 114.34 -623.54
    ## - factor(language)     1    0.3501 114.66 -622.24
    ## - cls_students         1    0.3923 114.70 -622.07
    ## <none>                             114.31 -621.66
    ## - age                  1    1.1818 115.49 -618.90
    ## - factor(ethnicity)    1    1.4771 115.78 -617.71
    ## - factor(gender)       1    3.0515 117.36 -611.46
    ## - cls_perc_eval        1    3.4284 117.74 -609.98
    ## - bty_avg              1    3.4287 117.74 -609.97
    ## - factor(cls_credits)  1    4.8017 119.11 -604.61
    ## 
    ## Step:  AIC=-623.91
    ## score ~ factor(ethnicity) + factor(gender) + factor(language) + 
    ##     age + cls_perc_eval + cls_students + factor(cls_level) + 
    ##     factor(cls_profs) + factor(cls_credits) + bty_avg
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## - factor(cls_profs)    1    0.0103 114.75 -625.87
    ## - factor(cls_level)    1    0.0173 114.76 -625.84
    ## - cls_students         1    0.3645 115.11 -624.44
    ## <none>                             114.74 -623.91
    ## - factor(language)     1    0.5568 115.30 -623.67
    ## - age                  1    0.8918 115.63 -622.32
    ## - factor(ethnicity)    1    1.7046 116.44 -619.08
    ## - factor(gender)       1    3.1469 117.89 -613.38
    ## - cls_perc_eval        1    3.5245 118.27 -611.90
    ## - bty_avg              1    3.5642 118.31 -611.75
    ## - factor(cls_credits)  1    5.6754 120.42 -603.56
    ## 
    ## Step:  AIC=-625.87
    ## score ~ factor(ethnicity) + factor(gender) + factor(language) + 
    ##     age + cls_perc_eval + cls_students + factor(cls_level) + 
    ##     factor(cls_credits) + bty_avg
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## - factor(cls_level)    1    0.0162 114.77 -627.80
    ## - cls_students         1    0.3731 115.12 -626.36
    ## <none>                             114.75 -625.87
    ## - factor(language)     1    0.5552 115.31 -625.63
    ## - age                  1    0.8964 115.65 -624.27
    ## - factor(ethnicity)    1    1.8229 116.57 -620.57
    ## - factor(gender)       1    3.1375 117.89 -615.38
    ## - cls_perc_eval        1    3.5166 118.27 -613.89
    ## - bty_avg              1    3.5547 118.31 -613.74
    ## - factor(cls_credits)  1    5.8278 120.58 -604.93
    ## 
    ## Step:  AIC=-627.8
    ## score ~ factor(ethnicity) + factor(gender) + factor(language) + 
    ##     age + cls_perc_eval + cls_students + factor(cls_credits) + 
    ##     bty_avg
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## - cls_students         1    0.3569 115.12 -628.36
    ## <none>                             114.77 -627.80
    ## - factor(language)     1    0.5390 115.31 -627.63
    ## - age                  1    0.8828 115.65 -626.25
    ## - factor(ethnicity)    1    1.8948 116.66 -622.22
    ## - factor(gender)       1    3.1222 117.89 -617.37
    ## - cls_perc_eval        1    3.5266 118.29 -615.79
    ## - bty_avg              1    3.5461 118.31 -615.71
    ## - factor(cls_credits)  1    6.2703 121.04 -605.17
    ## 
    ## Step:  AIC=-628.36
    ## score ~ factor(ethnicity) + factor(gender) + factor(language) + 
    ##     age + cls_perc_eval + factor(cls_credits) + bty_avg
    ## 
    ##                       Df Sum of Sq    RSS     AIC
    ## <none>                             115.12 -628.36
    ## - factor(language)     1    0.6192 115.74 -627.88
    ## - age                  1    0.9342 116.06 -626.62
    ## - factor(ethnicity)    1    1.8997 117.02 -622.79
    ## - cls_perc_eval        1    3.1769 118.30 -617.76
    ## - factor(gender)       1    3.4709 118.59 -616.61
    ## - bty_avg              1    4.0096 119.13 -614.51
    ## - factor(cls_credits)  1    6.1046 121.23 -606.44

``` r
back2$anova
```

    ##                  Step Df   Deviance Resid. Df Resid. Dev       AIC
    ## 1                     NA         NA       450   114.3082 -621.6578
    ## 2      - factor(rank)  2 0.43248627       452   114.7407 -623.9093
    ## 3 - factor(cls_profs)  1 0.01026963       453   114.7509 -625.8679
    ## 4 - factor(cls_level)  1 0.01624303       454   114.7672 -627.8023
    ## 5      - cls_students  1 0.35692695       455   115.1241 -628.3647

``` r
#final model 

back2$coefficients
```

    ##                   (Intercept) factor(ethnicity)not minority 
    ##                   3.446966644                   0.204710402 
    ##            factor(gender)male   factor(language)non-english 
    ##                   0.184779926                  -0.161463206 
    ##                           age                 cls_perc_eval 
    ##                  -0.005007522                   0.005094445 
    ## factor(cls_credits)one credit                       bty_avg 
    ##                   0.515065333                   0.064995939

being a male is beneficial to score about 0.18 to score while other
things are equal. one unit increase in bty_avg is related in 0.06
increase in the score while other things are equal.

typically, not being an moniroty,being male, engislh, and young, and one
credit, and good looking professors get higher scores.

No. Need more data from different university, region..etc.
