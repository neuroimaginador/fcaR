## Test environments
* local R installation, R 4.0.2
* win-builder (devel)
* macos-latest (release), windows-latest (release), ubuntu-16.04 (release and devel) via GitHub actions.
* local R with valgrind using docker
* Default platforms using devtools::check_rhub().

## Comments
This version addresses the issues in the previous (1.0.6) CRAN version. When tested using valgrind, no errors related to fcaR code are found.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

0 errors | 0 warnings | 0 note
