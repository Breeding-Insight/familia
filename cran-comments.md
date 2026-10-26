## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update to Familia 2.0.0, the version on CRAN. Version 2.1.0 was
  not released on CRAN, so its changes are part of this update.

## Dependencies

This version requires 'BIGpopA' (>= 2.2.0), available on CRAN since 2026-10-12.
'vcfR' stays in Imports: 'BIGpopA' reads VCF files with it but only suggests it.
'LEA' (Bioconductor) is in Suggests and is only needed by the SNMF tab, which
checks that it is installed.

# Familia 2.2.0
* Every tab has an "Example files" link that downloads a small simulated data
  set installed in `inst/extdata`.
* Find Parentage: added a Top N input, a `tied_candidates` ambiguity count, and
  a "Fill in pedigree" method that fills missing parents of a 3-column
  pedigree (disabled unless the progeny file is a pedigree), with an optional
  founders file.
* Validate Pedigree: added a Marker Summary table (per-marker mismatch counts)
  and a Marker Trio Table, both included in the download.
* PolyBreedTools: results include the model fit (R2) of each sample.
* SNMF: polyploid VCF files are converted with the same reader as the other
  tabs (ploidy up to 8); they used to fail.
* Fixed the "Check for Updates" dialog and the logo in the top bar on Linux.
* Validate Pedigree and Find Parentage no longer write `Rplots.pdf` to the
  working directory.
* Removed unused internal code and imports.
* Updated in-app help for Validate Pedigree and the other tabs to match the current behavior.

# Familia 2.1.0
* Find Parentage, Validate Pedigree and PolyBreedTools now accept genotypes as
  text files, VCF files, or 'PLINK' .ped files (with .map files in
  PolyBreedTools).
* PolyBreedTools: added an assignment threshold (default 50%); samples below
  it are labeled "Undetermined" (0 always assigns the highest line).
* SNMF: results use the lowest cross-entropy run by default; the manual run
  selector moved to an "Advanced Options" dialog.
* Updated in-app help for the new options.
* Fixed plot image downloads; 'svglite' is now in Suggests and used for svg
  export when available.

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
