## Revision purpose
AeroSampleR v0.1.14 adds a vignette and improved docummentation. Also, magrittr pipe operators were changed to the R base version. Due to the pipe change, the reports were revised to work around dot issues with purrr functions.

## Test environments
* local Windows 11 with 2022-07 Cumulative Update for Windows 11 for x64-based Systems (KB5015814) version R 4.2.1
* passed all checks with devtools::rhub::check()
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs. There were no NOTES except that New.md was in the top level directory. This was never noted until it was removed from the .Rbuildignore file, which I did based on a Warning that came up in the previous, aborted, release.

There were two R-hub checks with a NOTE and one with no notes
The notes were: 
* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found  
