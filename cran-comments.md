## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update to a package already on CRAN
  (version 1.0.3, published 2026-07-29).

# Familia 1.0.4
* Updated the funding attribution banner on the Home tab to read
  "University of Florida" instead of "Cornell University", reflecting the
  maintainer's current institution.
* Added URL and BugReports fields to DESCRIPTION.

## Note on submission timing

This update follows 1.0.3 more closely than the usual 1-2 month interval.
The released version displays an incorrect funding attribution in the running
application: it credits Cornell University, but the USDA-ARS award supporting
Breeding Insight is administered through University of Florida. We would like
to correct this misattribution promptly rather than leave it on CRAN for
another release cycle. Apologies for the quick turnaround.

# Familia 1.0.3
* This is a resubmission addressing the CRAN reviewer's comments. In this version I have:
    * Removed the redundant "R" from the start of the Title.
    * Written package/software names ('shiny', 'LEA') in single quotes in the Title and Description.
    * Added references describing the methods to the Description field in the authors (year) <doi:...> form.
    * Replaced the \dontrun{} wrapper with if(interactive()){} in the run_app() example.

# Familia 1.0.2
*Fixed issue with test directory to pass automated CRAN checks

# Familia 1.0.1
* This is a new submission.
