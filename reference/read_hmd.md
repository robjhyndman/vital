# Read data directly from HMD and construct a `vital` object for use in other functions

`read_hmd` reads single-year and single-age data from the Human
Mortality Database (HMD <https://www.mortality.org>) and constructs a
`vital` object suitable for use in other functions. This function uses
[`HMDHFDplus::readHMDweb()`](https://rdrr.io/pkg/HMDHFDplus/man/readHMDweb.html)
to download the required data. It is designed to handle Deaths,
Population, Exposure, Death Rates and Births. By default, Deaths,
Population, Exposure and Death Rates are downloaded. It is better to
handle Births separately as they are not age-specific.

## Usage

``` r
read_hmd(
  country,
  username,
  password,
  variables = c("Deaths", "Exposures", "Population", "Mx")
)
```

## Arguments

- country:

  Country name or country code as specified by the HMD. For instance,
  Australian data can be obtained using `country = "Australia"` or
  `country = "AUS"`.

- username:

  HMD username (case-sensitive)

- password:

  HMD password (case-sensitive)

- variables:

  List of variables to download from the HMD. If the data contains a
  mixture of age-specific and non-age-specific variables, then the
  non-age-specific data will be repeated for each age.

## Value

`read_hmd` returns a `vital` object combining the downloaded data.

## Details

In order to read the data, users are required to create an account with
the HMD website (<https://www.mortality.org>), and obtain a valid
username and password.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
norway <- read_hmd(
  country = "Norway",
  username = "Nora.Weigh@mymail.com",
  password = "FF!5xeEFa6"
)
norway_births <- read_hmd(
  country = "Norway",
  username = "Nora.Weigh@mymail.com",
  password = "FF!5xeEFa6",
  variables = "Births"
)
} # }
```
