## Test environments
* local R installation, on MACOS Sonoma 14.2, R version 4.3.0 (2023-04-21), platform: aarch64-apple-darwin20 (64-bit)
* rhub::rhub_check() with platforms: clang-asan, clang-ubsan, ubuntu-next, ubuntu-release, valgrind, linux (All R versions on GitHub Actions ubuntu-latest), m1-san (All R versions on GitHub Actions macos-15, ASAN + UBSAN on macOS), macos-arm64 (All R versions on GitHub Actions macos-latest), windows (All R versions on GitHub Actions windows-latest). All passed.
* windows-latest (release), ubuntu-latest (devel) and ubuntu-latest (release) via GitHub Actions.
* Using R-devel with win-builder.r-project.org using devtools::check_win_devel().

## Comments
New version with some improvements and several fixes for changes in other dependencies.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

0 errors | 0 warnings | 0 notes
