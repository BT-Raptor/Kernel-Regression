Thingamajig
================
BT_Raptor
21/04/2022

``` r
require(data.table)
```

    ## Loading required package: data.table

``` r
require(stats)
setwd("C:/Users/ricke/Desktop/Raptor's Data Science/")
options(scipen=999)
```

Data Acquisition

``` r
all_data=fread("opp.csv")
all_data=na.omit(all_data)
head(all_data)
```

    ##    case site Pop sex age hdlngth skullw totlngth taill footlgth earconch  eye
    ## 1:    1    1 Vic   m   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2
    ## 2:    2    1 Vic   f   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0
    ## 3:    3    1 Vic   f   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5
    ## 4:    4    1 Vic   f   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2
    ## 5:    5    1 Vic   f   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1
    ## 6:    6    1 Vic   f   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2
    ##    chest belly
    ## 1:  28.0    36
    ## 2:  28.5    33
    ## 3:  30.0    34
    ## 4:  28.0    34
    ## 5:  28.5    33
    ## 6:  30.0    32

Creating a new dataframe, where we do the column tweaking

``` r
all_data=all_data[,`:=`(case=NULL,site=NULL,Pop=NULL,sex=NULL)]
head(all_data)
```

    ##    age hdlngth skullw totlngth taill footlgth earconch  eye chest belly
    ## 1:   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2  28.0    36
    ## 2:   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0  28.5    33
    ## 3:   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5  30.0    34
    ## 4:   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2  28.0    34
    ## 5:   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1  28.5    33
    ## 6:   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2  30.0    32

Training & Test Split

``` r
train_dt=all_data[seq(1,52),]
test_dt=all_data[seq(53,104),]
test_dt=na.omit(test_dt)
```

summary of model

``` r
require(np)
```

    ## Loading required package: np

    ## Warning: package 'np' was built under R version 4.1.3

    ## Warning in .recacheSubclasses(def@className, def, env): undefined subclass
    ## "packedMatrix" of class "replValueSp"; definition not updated

    ## Warning in .recacheSubclasses(def@className, def, env): undefined subclass
    ## "packedMatrix" of class "mMatrix"; definition not updated

    ## Nonparametric Kernel Methods for Mixed Datatypes (version 0.60-11)
    ## [vignette("np_faq",package="np") provides answers to frequently asked questions]
    ## [vignette("np",package="np") an overview]
    ## [vignette("entropy_np",package="np") an overview of entropy-based methods]

``` r
model=npreg(age~hdlngth+skullw+totlngth+taill+footlgth+earconch+eye+chest+belly,data=train_dt)
```

    ## Multistart 1 of 5 |Multistart 1 of 5 |Multistart 1 of 5 |Multistart 1 of 5 /Multistart 1 of 5 -Multistart 1 of 5 \Multistart 1 of 5 |Multistart 1 of 5 /Multistart 1 of 5 -Multistart 1 of 5 \Multistart 1 of 5 |Multistart 1 of 5 |Multistart 1 of 5 |Multistart 1 of 5 /Multistart 1 of 5 -Multistart 1 of 5 \Multistart 1 of 5 |Multistart 1 of 5 /Multistart 2 of 5 |Multistart 2 of 5 |Multistart 2 of 5 /Multistart 2 of 5 -Multistart 2 of 5 \Multistart 2 of 5 |Multistart 2 of 5 /Multistart 2 of 5 -Multistart 2 of 5 |Multistart 2 of 5 |Multistart 2 of 5 /Multistart 2 of 5 -Multistart 3 of 5 |Multistart 3 of 5 |Multistart 3 of 5 /Multistart 3 of 5 -Multistart 3 of 5 \Multistart 3 of 5 |Multistart 3 of 5 /Multistart 3 of 5 -Multistart 3 of 5 \Multistart 3 of 5 |Multistart 3 of 5 |Multistart 3 of 5 /Multistart 3 of 5 -Multistart 3 of 5 \Multistart 3 of 5 |Multistart 4 of 5 |Multistart 4 of 5 |Multistart 4 of 5 /Multistart 4 of 5 -Multistart 4 of 5 \Multistart 4 of 5 |Multistart 4 of 5 /Multistart 4 of 5 -Multistart 4 of 5 \Multistart 4 of 5 |Multistart 4 of 5 |Multistart 4 of 5 /Multistart 4 of 5 -Multistart 4 of 5 \Multistart 5 of 5 |Multistart 5 of 5 |Multistart 5 of 5 /Multistart 5 of 5 -Multistart 5 of 5 \Multistart 5 of 5 |Multistart 5 of 5 /Multistart 5 of 5 -Multistart 5 of 5 \Multistart 5 of 5 |Multistart 5 of 5 |Multistart 5 of 5 |Multistart 5 of 5 /Multistart 5 of 5 -Multistart 5 of 5 \Multistart 5 of 5 |                   

``` r
summary(model)
```

    ## 
    ## Regression Data: 52 training points, in 9 variable(s)
    ##                hdlngth   skullw totlngth     taill footlgth earconch       eye
    ## Bandwidth(s): 47900370 61058356 4.334878 0.4367859 86224031 2.969759 0.6707073
    ##                  chest    belly
    ## Bandwidth(s): 1.022992 1.048675
    ## 
    ## Kernel Regression Estimator: Local-Constant
    ## Bandwidth Type: Fixed
    ## Residual standard error: 0.4267822
    ## R-squared: 0.9630064
    ## 
    ## Continuous Kernel Type: Second-Order Gaussian
    ## No. Continuous Explanatory Vars.: 9

Preds

``` r
test_p_dt=test_dt[,c('hdlngth','skullw','totlngth','taill','footlgth','earconch','eye','chest','belly')]
preds=sapply(predict(model,newdata=test_p_dt),round)
comparison=data.frame(test_dt$age,preds)
head(comparison)
```

    ##   test_dt.age preds
    ## 1           3     3
    ## 2           4     3
    ## 3           3     8
    ## 4           2     8
    ## 5           2     4
    ## 6           7     2
