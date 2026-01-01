# Generalized APC stochastic mortality model

A Generalized Age-Period-Cohort (GAPC) stochastic mortality mode is
defined in Villegas et al. (2018). The StMoMo package is used to fit the
model. Separate functions are available to fit various special cases of
the GAPC model.

## Usage

``` r
GAPC(formula, use_weights = TRUE, clip = 0, zeroCohorts = NULL, ...)

LC2(
  formula,
  link = c("log", "logit"),
  const = c("sum", "last", "first"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)

CBD(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)

RH(
  formula,
  link = c("log", "logit"),
  cohortAgeFun = c("1", "NP"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)

APC(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)

M7(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)

PLAT(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
)
```

## Arguments

- formula:

  Model specification

- use_weights:

  If `TRUE`, will call
  [`genWeightMat`](https://rdrr.io/pkg/StMoMo/man/genWeightMat.html)
  with arguments `clip` and `zeroCohorts`.

- clip:

  Passed to
  [`genWeightMat()`](https://rdrr.io/pkg/StMoMo/man/genWeightMat.html)

- zeroCohorts:

  Passed to
  [`genWeightMat()`](https://rdrr.io/pkg/StMoMo/man/genWeightMat.html)

- ...:

  All other arguments passed to
  [`StMoMo()`](https://rdrr.io/pkg/StMoMo/man/StMoMo.html)

- link:

  The link function to use. Either "log" or "logit". When using logit,
  the mortality rates need to be between 0 and 1. If they are not, most
  likely you need to use initial rather than central population values
  when computing them.

- const:

  defines the constraint to impose on the period index of the model
  ensure identifiability. The alternatives are "sum" (default), "last"
  and "first" which apply constraints \\\sum\_{t=1}^T \kappa_t=0\\,
  \\\kappa_T = 0\\ and \\\kappa_1 = 0\\ respectively.

- cohortAgeFun:

  A function defining the cohort age modulating parameter
  \\\beta_x^{(0)}\\. It can take values: "NP" for a non-parametric age
  term or "1" for \\\beta_x^{(0)} = 1\\ (the default).

## Value

A model specification.

## Details

`LC2()` provides an alternative implementation of the Lee-Carter model
based on a GAPC specification. The advantage of this approach over
[`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) is that it
allows for 0 rates in the mortality data. Note that it does not return
identical results to
[`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) because the
model formulation is different. For `LC2()`, do not take logs of the
mortality rates because this is handled with the link function. For
[`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md), you need to
take logs of the mortality rates when calling the function.

The Renshaw-Haberman (RH) model due to Renshaw and Haberman (2006) is
another special case of a GAPC model, that can be considered an
extension of a Lee-Carter model with an age-specific cohort effect.

The Age-Period-Cohort (APC) model is a special case of a GAPC model
discussed by Renshaw and Haberman (2011).

The Cairns-Blake-Dowd (CBD) model due to Cairns et al (2006) can be
considered another special case of a GAPC model that is primarily
intended for forecasting mortality patterns in older populations.

Cairns et al (2009) extended the CBD model by adding a cohort effect and
a quadratic age effect, giving the M7 model.

Plat (2009) combined the CBD model with some features of the Lee-Carter
model to produce a model that is suitable for full age ranges and
captures the cohort effect.

Each of these functions returns a GAPC model applied to the formula's
response variable as a function of age. The model will optionally call
[`genWeightMat`](https://rdrr.io/pkg/StMoMo/man/genWeightMat.html) with
arguments `clip` and `zeroCohorts`. All other arguments are passed to
[`StMoMo`](https://rdrr.io/pkg/StMoMo/man/StMoMo.html).

## References

Cairns, AJG, Blake, D, and Dowd, K (2006). A two-factor model for
stochastic mortality with parameter uncertainty: Theory and calibration.
*Journal of Risk and Insurance*, **73**(4), 687-718.
<doi:10.1111/j.1539-6975.2006.00195.x>

Cairns AJG, Blake D, Dowd K, Coughlan GD, Epstein D, Ong A, Balevich I
(2009). A quantitative comparison of stochastic mortality models using
data from England and Wales and the United States. *North American
Actuarial Journal*, **13**(1), 1–35.
<doi:10.1080/10920277.2009.10597538>

Lee, RD, and Carter, LR (1992) Modeling and forecasting US mortality.
*Journal of the American Statistical Association*, 87, 659-671.
<doi:10.1080/01621459.1992.10475265>

Plat R (2009). On stochastic mortality modeling. *Insurance: Mathematics
and Economics*, **45**(3), 393–404.
<doi:10.1016/j.insmatheco.2009.08.006>

Renshaw, AE, and Haberman, S (2006). A cohort-based extension to the
Lee-Carter model for mortality reduction factors. *Insurance:
Mathematics and Economics*, **38**(3), 556-570.
<doi:10.1016/j.insmatheco.2005.12.001>

Renshaw, AE, and Haberman, S (2011). A comparative study of parametric
mortality projection models. *Insurance: Mathematics and Economics*,
**48**(1), 35–55. \<doi:10.1016/j. insmatheco.2010.09.003\>

Villegas, AM, Millossovich, P, and Kaishev, VK (2018). StMoMo: An R
package for stochastic mortality modelling. *Journal of Statistical
Software*, **84**(3), 1-38. <doi:10.18637/jss.v084.i03>

## See also

[`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md)

## Author

Rob J Hyndman

## Examples

``` r
# Fit the same CBD model using GAPC() and CBD()
gapc <- norway_mortality |>
  dplyr::filter(Sex == "Female", Age > 50, Year > 2000) |>
  model(
    cbd1 = GAPC(Mortality,
      link = "log",
      staticAgeFun = FALSE,
      periodAgeFun = c("1", function(x, ages) x - mean(ages))
    ),
    cbd2 = CBD(Mortality)
  )
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo 
#> Warning: StMoMo: 8 data points have 
#>                   non-positive exposures and have been zero weighted
#> Warning: StMoMo: 8 data points have 
#>                   non-positive exposures and have been zero weighted
glance(gapc)
#> # A tibble: 2 × 6
#>   Sex    .model loglik deviance  nobs  npar
#>   <chr>  <chr>   <dbl>    <dbl> <int> <int>
#> 1 Female cbd1   -7766.    6056.  1345    46
#> 2 Female cbd2   -7766.    6056.  1345    46
gapc |>
  dplyr::select(cbd2) |>
  report()
#> Series: Mortality 
#> Model: GAPC 
#> Stochastic Mortality Model fit
#> Call: fit.StMoMo(object = model, Dxt = data2$Dxt, Ext = data2$Ext,  
#> Call:     ages = data2$ages, years = data2$years, wxt = wxt, verbose = FALSE)
#> 
#> Poisson model with predictor: log m[x,t] = k1[t] + f2[x] k2[t]
#> 
#> Data:  vital
#> Series:  Female
#> Years in fit: 2001 - 2023
#> Ages in fit: 51 - 110 
#> 
#> Log-likelihood:  -7765.57
#> Deviance:  6055.75
#> Number of parameters:  46
```
