## Revision purpose
AeroSampleR v0.1.14 adds a vignette and improved docummentation. Also, magrittr pipe operators were changed to the R base version. Due to the pipe change, the reports were revised to work around dot issues with purrr functions.

## Test environments
* local Windows 11 with 2022-07 Cumulative Update for Windows 11 for x64-based Systems (KB5015814) version R 4.2.1
* passed all checks with devtools::rhub::check()
) and devtools::check_win_devel()

## R CMD check results
There were no ERRORs. There were two NOTES: 1. News.md was in the top level directory. This was never noted until it was removed from the .Rbuildignore file, which I did based on a Warning that came up in the previous, aborted, release. 2. Namespace in Imports field not imported from: 'flextable'. The flextable package is used in the vignette. The Note persists whether or not it is in the imports section of the DESCRIPTION.

## win-builder check results
There were two NOTES:
1. I changed my email address because I can't access github from behind my work address firewall.
* Not a note, but the doi link failed to check because service was unavailable.
2. News.md at the top level. I think this is okay now.


## R-hub check results
There were four NOTES: 
1. I changed my email address because I can't access github from behind my work address firewall.
* Not a note, but the doi link failed to check because service was unavailable.
2. News.md at the top level. I think this is okay now.
3. Namespasce in Imports field not imported from 'flextable'. All declared imports should be used. Yes, but I use it in the vignette.
4. checking HTML version of manual ... NOTE Skipping checking HTML validation:: no command 'tidy' found. I don't know that this is a problem. Please advise if it is.
