
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vital

<!-- badges: start -->

<!-- badges: end -->

The goal of vital is to allow analysis of demographic data using tidy
tools.

## Installation

You can install the development version of vital from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robjhyndman/vital")
```

## Example

``` r
library(vital)
```

``` r
demography::fr.mort |> as_vital()
#> # A tsibble: 63,603 x 7 [1Y]
#> # Key:       AgeGroup, Age, Group [333]
#>     Year AgeGroup   Age Group  Mortality Exposure Deaths
#>    <dbl> <chr>    <dbl> <chr>      <dbl>    <dbl>  <dbl>
#>  1  1816 0            0 female     0.187  408224. 76332.
#>  2  1817 0            0 female     0.182  416591. 75706.
#>  3  1818 0            0 female     0.186  406352. 75465.
#>  4  1819 0            0 female     0.197  414025. 81477.
#>  5  1820 0            0 female     0.181  442196. 80000.
#>  6  1821 0            0 female     0.182  438347. 79709.
#>  7  1822 0            0 female     0.207  418536. 86756.
#>  8  1823 0            0 female     0.192  418807. 80469.
#>  9  1824 0            0 female     0.199  423222. 84023.
#> 10  1825 0            0 female     0.194  448137. 87104.
#> # … with 63,593 more rows
```
