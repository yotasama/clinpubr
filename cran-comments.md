## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Resubmission
This is a resubmission. In this version I have:

* Removed the trailing whitespace in the Description field of DESCRIPTION.

* Changed the default output options so that the functions do not write
  by default in the user's home filespace and removed default directories. 
  
* Removed the non-export function that contains `install.packages` in R/utils.R.

Clarification: Currently, there are no references describing the methods in
the package. However, a paper about this package is in preparation, and I 
will add the reference when it is published.