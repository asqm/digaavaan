
<!-- README.md is generated from README.Rmd. Please edit that file -->

# digavaan

The goal of digavaan is to extract information as R data structures from
a `lavaan` object.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asqm/digavaan")
```

**Warning**: `digavaan` will most likely never go to CRAN because it
uses internal functions from `lavaan`. This hinders the package as
unstable. However, because most of the inner functions that are used in
this package are the same used by `lavaan::summary`, unless there’s a
breaking change (very unlikely with such a mature package) the same
information should be stable.

## Example

Using the same example from `lavaan`:

``` r
library(lavaan)
#> This is lavaan 0.5-23.1097
#> lavaan is BETA software! Please report any bugs.
library(digavaan)

model <- "
    # latent variables
     ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4
      dem65 =~ y5 + y6 + y7 + y8
    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60
    # residual covariances
      y1 ~~ y5
      y2 ~~ y4 + y6
      y3 ~~ y7
      y4 ~~ y8
      y6 ~~ y8"

fit <- sem(model, data=PoliticalDemocracy)

# Traditional print
summary(fit)
#> lavaan (0.5-23.1097) converged normally after  68 iterations
#> 
#>   Number of observations                            75
#> 
#>   Estimator                                         ML
#>   Minimum Function Test Statistic               38.125
#>   Degrees of freedom                                35
#>   P-value (Chi-square)                           0.329
#> 
#> Parameter Estimates:
#> 
#>   Information                                 Expected
#>   Standard Errors                             Standard
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   ind60 =~                                            
#>     x1                1.000                           
#>     x2                2.180    0.139   15.742    0.000
#>     x3                1.819    0.152   11.967    0.000
#>   dem60 =~                                            
#>     y1                1.000                           
#>     y2                1.257    0.182    6.889    0.000
#>     y3                1.058    0.151    6.987    0.000
#>     y4                1.265    0.145    8.722    0.000
#>   dem65 =~                                            
#>     y5                1.000                           
#>     y6                1.186    0.169    7.024    0.000
#>     y7                1.280    0.160    8.002    0.000
#>     y8                1.266    0.158    8.007    0.000
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~                                             
#>     ind60             1.483    0.399    3.715    0.000
#>   dem65 ~                                             
#>     ind60             0.572    0.221    2.586    0.010
#>     dem60             0.837    0.098    8.514    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.624    0.358    1.741    0.082
#>  .y2 ~~                                               
#>    .y4                1.313    0.702    1.871    0.061
#>    .y6                2.153    0.734    2.934    0.003
#>  .y3 ~~                                               
#>    .y7                0.795    0.608    1.308    0.191
#>  .y4 ~~                                               
#>    .y8                0.348    0.442    0.787    0.431
#>  .y6 ~~                                               
#>    .y8                1.356    0.568    2.386    0.017
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                0.082    0.019    4.184    0.000
#>    .x2                0.120    0.070    1.718    0.086
#>    .x3                0.467    0.090    5.177    0.000
#>    .y1                1.891    0.444    4.256    0.000
#>    .y2                7.373    1.374    5.366    0.000
#>    .y3                5.067    0.952    5.324    0.000
#>    .y4                3.148    0.739    4.261    0.000
#>    .y5                2.351    0.480    4.895    0.000
#>    .y6                4.954    0.914    5.419    0.000
#>    .y7                3.431    0.713    4.814    0.000
#>    .y8                3.254    0.695    4.685    0.000
#>     ind60             0.448    0.087    5.173    0.000
#>    .dem60             3.956    0.921    4.295    0.000
#>    .dem65             0.172    0.215    0.803    0.422

# Same information but as a data frames
summary_lavaan(fit)
#> $header
#> $header$iter
#> [1] "lavaan (0.5-23.1097) converged normally after  68 iterations\n"
#> 
#> $header$n_groups
#>   name_group used total
#> 1    Group 1   75    NA
#> 
#> $header$estimator_details
#>                         estimator       ml ml_scaled
#> 1 Minimum Function Test Statistic 38.12522        NA
#> 2              Degrees of freedom 35.00000        NA
#> 3            P-value (Chi-square)  0.32900        NA
#> 
#> 
#> $estimates
#>      lhs op   rhs exo   est    se      z pvalue
#> 1  ind60 =~    x1   0 1.000 0.000     NA     NA
#> 2  ind60 =~    x2   0 2.180 0.139 15.742  0.000
#> 3  ind60 =~    x3   0 1.819 0.152 11.967  0.000
#> 4  dem60 =~    y1   0 1.000 0.000     NA     NA
#> 5  dem60 =~    y2   0 1.257 0.182  6.889  0.000
#> 6  dem60 =~    y3   0 1.058 0.151  6.987  0.000
#> 7  dem60 =~    y4   0 1.265 0.145  8.722  0.000
#> 8  dem65 =~    y5   0 1.000 0.000     NA     NA
#> 9  dem65 =~    y6   0 1.186 0.169  7.024  0.000
#> 10 dem65 =~    y7   0 1.280 0.160  8.002  0.000
#> 11 dem65 =~    y8   0 1.266 0.158  8.007  0.000
#> 12 dem60  ~ ind60   0 1.483 0.399  3.715  0.000
#> 13 dem65  ~ ind60   0 0.572 0.221  2.586  0.010
#> 14 dem65  ~ dem60   0 0.837 0.098  8.514  0.000
#> 15    y1 ~~    y5   0 0.624 0.358  1.741  0.082
#> 16    y2 ~~    y4   0 1.313 0.702  1.871  0.061
#> 17    y2 ~~    y6   0 2.153 0.734  2.934  0.003
#> 18    y3 ~~    y7   0 0.795 0.608  1.308  0.191
#> 19    y4 ~~    y8   0 0.348 0.442  0.787  0.431
#> 20    y6 ~~    y8   0 1.356 0.568  2.386  0.017
#> 21    x1 ~~    x1   0 0.082 0.019  4.184  0.000
#> 22    x2 ~~    x2   0 0.120 0.070  1.718  0.086
#> 23    x3 ~~    x3   0 0.467 0.090  5.177  0.000
#> 24    y1 ~~    y1   0 1.891 0.444  4.256  0.000
#> 25    y2 ~~    y2   0 7.373 1.374  5.366  0.000
#> 26    y3 ~~    y3   0 5.067 0.952  5.324  0.000
#> 27    y4 ~~    y4   0 3.148 0.739  4.261  0.000
#> 28    y5 ~~    y5   0 2.351 0.480  4.895  0.000
#> 29    y6 ~~    y6   0 4.954 0.914  5.419  0.000
#> 30    y7 ~~    y7   0 3.431 0.713  4.814  0.000
#> 31    y8 ~~    y8   0 3.254 0.695  4.685  0.000
#> 32 ind60 ~~ ind60   0 0.448 0.087  5.173  0.000
#> 33 dem60 ~~ dem60   0 3.956 0.921  4.295  0.000
#> 34 dem65 ~~ dem65   0 0.172 0.215  0.803  0.422
```
