## Revision purpose
AeroSampleR v0.3.0 removes a function option that sometimes fails.

## Test environments
* local Windows 11 home version 25H2
* passed all checks with devtools::rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs, WARNINGS when running devtools::check() with default parameters. There was one NOTE regarding the use of the flextable package. This package is used only in the vignette and the check notes that it is not used in any functions.
