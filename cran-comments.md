## Test environments
* local R installation, on MACOS BigSur 11.4, R version 4.1.0 RC (2021-05-17 r80314), platform: aarch64-apple-darwin20 (64-bit)
* win-builder (devel)
* macos-latest (release), windows-latest (release), ubuntu-16.04 (release) via GitHub actions.
* Debian Linux, R-devel, GCC ASAN/UBSAN, and Ubuntu Linux 20.04.1 LTS, R-release, GCC on R-hub.
* local R with valgrind using docker
* Default platforms using devtools::check_rhub().

## Comments
This version is just a minor update fixing some minor bugs that appeared in some specific cases.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

0 errors | 0 warnings | 0 note
