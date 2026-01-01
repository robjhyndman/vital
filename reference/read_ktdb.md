# Read old-age mortality from Kannisto-Thatcher (K-T) database and construct a `vital` object for use in other functions

`read_ktdb` reads old-age mortality data classified by sex, age, year of
birth, and calendar year for more than 30 countries. The series is
available in Kannisto-Thatcher (K-T) database
(<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>) and
constructs a `vital` object suitable for use in other functions.

## Usage

``` r
read_ktdb(country, triangle = 1)
```

## Arguments

- country:

  Country name or country code as specified by the KT database. For
  instance, Australian data can be obtained using
  `country = "Australia"` or `country = 1`.

- triangle:

  Lexis triangle number, 1 (default) is lower triangle, 2 is upper
  triangle.

## Value

`read_ktdb` returns a `vital` object combining the downloaded data.

## Author

Sixian Tang

## Examples

``` r
if (FALSE) { # \dontrun{
australia <- read_ktdb(country = "Australia")
} # }
```
