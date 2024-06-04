
Re email response to first submission:

* There are references for specific methods in the individual help files. Because there are so many methods implemented, it is better to put these in the help files, rather than in the DESCRIPTION file.
* The only \dontrun{} uses are for examples that download data using a specific (bogus) username and password, or which read specific files that the user has downloaded from relevant websites. These will therefore error, and so I can't use \donttest{} instead. 
* All other requested changes have been made.

## Test environments

* KDE neon 6.0 based on ubuntu 22.04 (local): R 4.4.1
* macOS (on GitHub Actions): release
* windows (on GitHub Actions): release
* ubuntu 22.04.4 (on GitHub Actions): devel, release, oldrel
* win-builder: devel, release, oldrelease

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
