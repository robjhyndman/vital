# Read Short-Term Mortality Fluctuations data from the Human Mortality Database

`read_stmf` reads weekly mortality data from the Short-term Mortality
Fluctuations (STMF) series available in the Human Mortality Database
(HMD) <https://www.mortality.org/Data/STMF>), and constructs a `vital`
object suitable for use in other functions.

## Usage

``` r
read_stmf(country)
```

## Arguments

- country:

  Country name or country code as specified by the HMD. For instance,
  Australian data can be obtained using `country = "Australia"` or
  `country = "AUS"`.

## Value

A `vital` object combining the downloaded data.

## Author

Sixian Tang

## Examples

``` r
if (FALSE) { # \dontrun{
norway <- read_stmf(country = "NOR")
} # }

```
