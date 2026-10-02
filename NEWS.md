# Familia 2.1.0

* Find Parentage, Validate Pedigree and PolyBreedTools now accept genotypes as
  text files (.txt/.csv), VCF files (.vcf/.vcf.gz), or 'PLINK' .ped files, in
  addition to the previous tab-separated format. VCF calls are converted to
  allele-B dosages using the selected Ploidy; .ped files are diploid only.
* PolyBreedTools: a .map upload appears for each .ped genotype file so markers
  are matched by name. If either genotype file is a .ped, both must be; the
  validation .ped is coded with the reference panel's counted allele per marker.
* PolyBreedTools: new **Assignment threshold (%)** input. Samples whose highest
  line proportion is below the threshold are labelled "Undetermined". Changes
  apply without re-running the estimation. Default 0 keeps the previous behavior.
* SNMF: the plot, Q matrix and downloads use the best (lowest cross-entropy)
  run for the selected K by default. The manual run selector moved to an
  **Advanced Options** dialog for diagnostics; changing K or re-running resets
  to the best run. The Help panel now explains run selection.
* Updated the in-app Instructions and Help panels for the new input formats.
* Column names `id`, `male_parent`, `female_parent` and `sex` in uploaded files
  are no longer case sensitive (e.g. `ID`, `Male_Parent` are accepted).
* Requires 'BIGpopA' (>= 2.1.0).

# Familia 2.0.0

* Added a **Ploidy** selector to the Find Parentage and Validate Pedigree tabs,
  passed through to 'BIGpopA' so pedigree validation and parentage assignment
  now support any ploidy. Even ploidy uses the full polysomic Mendelian test;
  odd ploidy (e.g. triploid) uses a homozygosity-based check. Default is 2 (diploid).
* Updated the in-app Instructions and Help panels to document the Ploidy option
  and to generalize genotype dosage coding to 0, 1, ..., ploidy.
* Requires 'BIGpopA' (>= 2.0.0).

# Familia 1.0.4

* Updated the funding attribution banner on the Home tab to read
  "University of Florida" instead of "Cornell University", reflecting the
  correct institution.
* Added `URL` and `BugReports` fields to DESCRIPTION.

# Familia 1.0.3

* Addressed CRAN reviewer feedback: removed the redundant "R" from the Title;
  wrapped software names ('shiny', 'LEA') in single quotes in the Title and
  Description; added method references to the Description (sNMF, the 'LEA'
  package, the breed-composition methods of Funkhouser et al. and Sandercock
  et al., and the 'BIGpopA' package) with DOIs or canonical URLs; and replaced
  the `\dontrun{}` example wrapper in `run_app()`
  with `if(interactive()){}`.

# Familia 1.0.2

*Fixed test directory for CRAN submission

# Familia 1.0.1

* Prepared app for CRAN submission.
* Replaced the `tidyverse` meta-package dependency with `dplyr` and `tidyr`.
* Declared `ggplot2`, `curl`, and `httr` in Imports and removed the unused `viridis` dependency.
* Removed the `Remotes` field now that `BIGpopA` is available on CRAN.

# Familia 0.1.0

* Initial Golem framework
