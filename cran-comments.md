## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Resubmission
This is a resubmission. In this version I have:

* Removed 1 tailing space in the Description field of DESCRIPTION.

* Changed the default options so that the functions do not write by default
  in the user's home filespace and removed default directories. 
  
* The non-export function that contains `install.packages` in R/utils.R was
  removed.