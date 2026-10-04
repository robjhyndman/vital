# Read STMF data from files downloaded from HMD

`read_stmf_files` reads weekly mortality data from a file downloaded
from the Short-term Mortality Fluctuations (STMF) series available in
the Human Mortality Database (HMD)
<https://www.mortality.org/Data/STMF>, and constructs a `vital` object
suitable for use in other functions.

## Usage

``` r
read_stmf_files(file)
```

## Arguments

- file:

  Name of a file containing data downloaded from the HMD.

## Value

`read_stmf_files` returns a `vital` object combining the downloaded
data.

## See also

[`read_stmf()`](https://pkg.robjhyndman.com/vital/reference/read_stmf.md)
for downloading and reading STMF data directly from the HMD.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
# File downloaded from the Human Mortality Database STMF series
# (https://www.mortality.org/Data/STMF)
mortality <- read_stmf_files("AUSstmfout.csv")
} # }
```
