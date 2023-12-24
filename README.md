
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vital <img src="man/figures/vital-hex.png" align="right" width = 150 />

<!-- badges: start -->

[![R-CMD-check](https://github.com/robjhyndman/vital/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/robjhyndman/vital/actions/workflows/R-CMD-check.yaml)
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
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

# Examples using Victorian females
vic_female <- aus_mortality |>
  filter(State == "Victoria", Sex == "female")

# Lifetable in 2000
vic_female |> 
  filter(Year == 2000) |>
  life_table()
#> # A vital: 101 x 12 [?]
#> # Key:     Age, Sex, State, Code [101]
#>     Year Sex   State Code       mx      qx    lx      dx    Lx    Tx    ex   Age
#>    <int> <chr> <chr> <chr>   <dbl>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <int>
#>  1  2000 fema… Vict… VIC   4.02e-3 4.01e-3 1     4.01e-3 0.996  82.6  82.6     0
#>  2  2000 fema… Vict… VIC   2.70e-4 2.70e-4 0.996 2.69e-4 0.996  81.6  82.0     1
#>  3  2000 fema… Vict… VIC   3.38e-5 3.38e-5 0.996 3.37e-5 0.996  80.6  81.0     2
#>  4  2000 fema… Vict… VIC   3.32e-5 3.32e-5 0.996 3.30e-5 0.996  79.6  80.0     3
#>  5  2000 fema… Vict… VIC   9.83e-5 9.83e-5 0.996 9.79e-5 0.996  78.7  79.0     4
#>  6  2000 fema… Vict… VIC   9.65e-5 9.65e-5 0.996 9.61e-5 0.996  77.7  78.0     5
#>  7  2000 fema… Vict… VIC   2.22e-4 2.22e-4 0.995 2.21e-4 0.995  76.7  77.0     6
#>  8  2000 fema… Vict… VIC   6.32e-5 6.32e-5 0.995 6.29e-5 0.995  75.7  76.0     7
#>  9  2000 fema… Vict… VIC   3.16e-5 3.16e-5 0.995 3.14e-5 0.995  74.7  75.0     8
#> 10  2000 fema… Vict… VIC   1.25e-4 1.25e-4 0.995 1.25e-4 0.995  73.7  74.0     9
#> # ℹ 91 more rows

# Life expectancy
vic_female |> 
  life_expectancy() |> 
  ggplot(aes(x = Year, y = ex)) +
  geom_line()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# Smoothed data
vic_female |> 
  filter(Year == 2000) |> 
  smooth_mortality(Mortality) |>
  autoplot(Mortality) +
  geom_line(aes(y = .smooth), col = "blue") +
  ylab("Mortality rate") +
  scale_y_log10()
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r

# Lee-Carter model
lc <- vic_female |>
  model(lee_carter = LC(log(Mortality)))
report(lc)
#> Series: Mortality 
#> Model: LC 
#> Transformation: log(Mortality) 
#> 
#> Options:
#>   Adjust method: dt
#>   Jump choice: fit
#> 
#> Age functions
#> # A tibble: 101 × 3
#>     Age    ax     bx
#>   <int> <dbl>  <dbl>
#> 1     0 -4.15 0.0157
#> 2     1 -6.40 0.0218
#> 3     2 -7.01 0.0195
#> 4     3 -7.32 0.0180
#> 5     4 -7.36 0.0159
#> # ℹ 96 more rows
#> 
#> Time coefficients
#> # A tsibble: 120 x 2 [1Y]
#>    Year    kt
#>   <int> <dbl>
#> 1  1901 111. 
#> 2  1902 111. 
#> 3  1903 109. 
#> 4  1904 100. 
#> 5  1905  98.8
#> # ℹ 115 more rows
#> 
#> Time series model: RW w/ drift 
#> 
#> Variance explained: 72.99%

autoplot(lc)
```

<img src="man/figures/README-example-3.png" width="100%" />

``` r

# Forecasts from Lee-Carter model
lc |>
  forecast(h = 20) |>
  autoplot(Mortality) +
  ylab("Mortality rate") +
  scale_y_log10()
```

<img src="man/figures/README-example-4.png" width="100%" />
