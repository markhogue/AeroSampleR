## Revision purpose
AeroSampleR v0.1.14 adds a vignette and improved docummentation. Also, magrittr pipe operators were changed to the R base version. Due to the pipe change, the reports were revised to work around dot issues with purrr functions.

## Test environments
* local Windows 11 with 2022-08 Cumulative Update for Windows 11 for x64-based Systems (KB5016629)
R 4.2.1

## R CMD check results
There were no ERRORs. There was one NOTES: 1. Namespace in Imports field not imported from: 'flextable'. The flextable package is used in the vignette. The Note persists whether or not it is in the imports section of the DESCRIPTION.

## win-builder check results
There was one NOTES:
1. I changed my email address because I can't access github from behind my work address firewall.

## R-hub check results
There were three NOTES: 
1. I changed my email address because I can't access github from behind my work address firewall.
2. Namespasce in Imports field not imported from 'flextable'. All declared imports should be used. Yes, but I use it in the vignette.
3. checking HTML version of manual ... NOTE Skipping checking HTML validation:: no command 'tidy' found. I don't know that this is a problem. Please advise if it is.
