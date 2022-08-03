## Revision purpose
This is the first issuance of AeroSampleR. v0.1.13 updates to add a vignette. Also, magrittr pipe operators were changed to the R base version. Possibly due to the pipe change, the reports were revised to work around dot issues with purrr functions.

## Test environments
* local Windows 11 with 2022-07 Cumulative Update for Windows 11 for x64-based Systems (KB5015814) version R 4.2.1
* passed all checks with devtools::rhub::check()
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs or NOTES.
There were two R-hub checks with a NOTE and one with no notes
The notes were: 
* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found  
