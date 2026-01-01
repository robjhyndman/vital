# Read data directly from HFD and construct a `vital` object for use in other functions

`read_hfd` reads single-year and single-age data from the Human
Fertility Database (HFD <https://www.humanfertility.org>) and constructs
a `vital` object suitable for use in other functions. This function uses
[`HMDHFDplus::readHFDweb()`](https://rdrr.io/pkg/HMDHFDplus/man/readHFDweb.html)
to download the required data. It is designed to handle age-specific
fertility rates. It may be extended to handle other types of data in the
future.

## Usage

``` r
read_hfd(country, username, password, variables = "asfrRR")
```

## Arguments

- country:

  Directory abbreviation from the HMD. For instance, Norway = "NOR".

- username:

  HFD username (case-sensitive)

- password:

  HFD password (case-sensitive)

- variables:

  List of variables to download from the HFD. By default, the
  age-specific fertility rate (asfrRR) is downloaded.

## Value

`read_hfd` returns a `vital` object combining the downloaded data.

## Details

In order to read the data, users are required to create an account with
the HFD website (<https://www.humanfertility.org>), and obtain a valid
username and password.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
norway <- read_hfd(
  country = "NOR",
  username = "Nora.Weigh@mymail.com",
  password = "FF!5xeEFa6"
)
} # }
```
