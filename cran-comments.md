## Revision purpose
This is the first issuance of AeroSampleR. v0.1.12 updates per CRAN comments - provided \action for plots and running examples

## Test environments
* local Windows 10 home version R 4.1.2
* passed all checks with devtools::rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs or NOTES, aside from the note identifying the maintainer, stating that this is a new submission and identifying suspect word spelling (but these are names that are spelled correctly). 
