# Read old-age mortality data from files downloaded from K-T database

`read_ktdb_files` reads old-age mortality data from files downloaded
from K-T database
(<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>) and
constructs a `vital` object suitable for use in other functions. If two
files are provided, the function will treat them as data for each
gender, returning a combined dataset. If only one file is provided, the
function will assume that it represents data for a single gender.

## Usage

``` r
read_ktdb_files(male = NULL, female = NULL, triangle = 1)
```

## Arguments

- male:

  File containing male mortality downloaded from the K-T database.

- female:

  File containing female mortality downloaded from the K-T database.

- triangle:

  Lexis triangle number, 1 (default) is lower triangle, 2 is upper
  triangle.

## Value

`read_ktdb_files` returns a `vital` object combining the downloaded
data.

## Author

Sixian Tang

## Examples

``` r
if (FALSE) { # \dontrun{
# File downloaded from the K-T database
australia_male <- read_ktdb_files("maustl.txt")
} # }
```
