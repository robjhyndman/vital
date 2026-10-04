# Read Short-Term Mortality Fluctuations data from the Human Mortality Database

`read_stmf` reads weekly mortality data from the Short-term Mortality
Fluctuations (STMF) series available in the Human Mortality Database
(HMD) <https://www.mortality.org/Data/STMF>, and constructs a `vital`
object suitable for use in other functions. The HMD requires a login to
download STMF data.

## Usage

``` r
read_stmf(country, username, password)
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

## Value

A `vital` object combining the downloaded data.

## See also

[`read_stmf_files()`](https://pkg.robjhyndman.com/vital/reference/read_stmf_files.md)
for reading STMF files that have already been downloaded.

## Author

Sixian Tang

## Examples

``` r
if (FALSE) { # \dontrun{
norway <- read_stmf(
  country = "NOR",
  username = "Nora.Weigh@mymail.com",
  password = "FF!5xeEFa6"
)
} # }
```
