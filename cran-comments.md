## Revision purpose
This is the first issuance of AeroSampleR.

## Test environments
* local Windows 10 home version R 4.1.2
* passed all checks with devtools::rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs or NOTES. 
