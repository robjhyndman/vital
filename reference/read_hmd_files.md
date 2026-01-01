# Read data from files downloaded from HMD and construct a `vital` object for use in other functions

`read_hmd_files` reads single-year and single-age data from files
downloaded from the Human Mortality Database (HMD
<https://www.mortality.org>) and constructs a `vital` object suitable
for use in other functions. This function uses
[`HMDHFDplus::readHMD()`](https://rdrr.io/pkg/HMDHFDplus/man/readHMD.html)
to parse the files.

## Usage

``` r
read_hmd_files(files)
```

## Arguments

- files:

  Vector of file names containing data downloaded from the HMD. The file
  names are used to determine what they contain. If the file names are
  as per the HMD, then the function will automatically determine the
  contents. If it is unclear what a file contains, the columns will be
  named according to the filename. If the data contains a mixture of
  age-specific and non-age-specific variables, then the non-age-specific
  data will be repeated for each age. If you have HMD files for many
  countries, all with the same names, then you should put them in
  separate folders to avoid confusion, and to save changing all the
  filenames.

## Value

`read_hmd_files` returns a `vital` object combining the downloaded data.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
# Files downloaded from the [Human Mortality Database](https://mortality.org)
mortality <- read_hmd_files(
  c("Deaths_1x1.txt", "Exposures_1x1.txt", "Population.txt", "Mx_1x1.txt")
)
births <- read_hmd_files("Births.txt")
} # }
```
