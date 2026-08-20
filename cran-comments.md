## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update to a package already on CRAN.

# Familia 2.0.0
* Added a Ploidy selector to the Find Parentage and Validate Pedigree tabs,
  enabling pedigree validation and parentage assignment for any ploidy through
  'BIGpopA' (>= 2.0.0). Even ploidy uses the full Mendelian test; odd ploidy
  uses a homozygosity-based check. Default behavior (diploid, ploidy = 2) is
  unchanged.

# Familia 1.0.4
* Updated the funding attribution banner on the Home tab to read
  "University of Florida" instead of "Cornell University".
* Added URL and BugReports fields to DESCRIPTION.

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
