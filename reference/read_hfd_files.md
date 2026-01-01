# Read data from files downloaded from HFD and construct a `vital` object for use in other functions

`read_hfd_files` reads single-year and single-age data from files
downloaded from the Human Mortality Database (HFD
<https://www.humanfertility.org>) and constructs a `vital` object
suitable for use in other functions. This function uses
[`HMDHFDplus::readHFD()`](https://rdrr.io/pkg/HMDHFDplus/man/readHFD.html)
to parse the files.

## Usage

``` r
read_hfd_files(files)
```

## Arguments

- files:

  Vector of file names containing data downloaded from the HFD. The file
  names are used to determine what they contain. If the file names are
  as per the HFD, then the function will automatically determine the
  contents. If it is unclear what a file contains, the columns will be
  named according to the filename. If the data contains a mixture of
  age-specific and non-age-specific variables, then the non-age-specific
  data will be repeated for each age. If you have HMD files for many
  countries, all with the same names, then you should put them in
  separate folders to avoid confusion, and to save changing all the
  filenames.

## Value

`read_hfd_files` returns a `vital` object combining the downloaded data.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
# File downloaded from the [Human Fertility Database](https://www.humanfertility.org)
fertility <- read_hfd_files("NORasfrRR.txt")
} # }
```
