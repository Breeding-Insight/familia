# Familia 2.2.0

* Every tab has an **Example files** link in its Inputs box that downloads a
  small simulated data set (`inst/extdata`, made by `data-raw/make_example_data.R`).
  It is the data set used in the user guide.
* Find Parentage: new optional **Founders File**, shown for *Fill in pedigree* and
  passed to the BIGpopA `founders_file` argument. Founders with both parents unknown are
  skipped; without the file a notification warns that every individual with both
  parents unknown, founders included, gets a full parent search.
* PolyBreedTools: the Results Table and the Excel file include `R2`, the model
  fit of each sample, explained in the Instructions and Help. The Instructions
  no longer say that the first column must be `ID` (any column named `ID`, in
  any case, is used). The Example Inputs box shows and downloads the example files.
* Validate Pedigree: the founders file also accepts `.csv` and `.tsv`, and the
  Instructions explain what it does.
* Help: the Best male / female parent panels showed `<code>` tags as text; the
  method panels now give the score each method uses.
* SNMF: VCF uploads are read with the same reader as the other tabs (BIGpopA
  `vcf_to_dosage()`, which drops multiallelic markers) and written as a LEA
  `.geno` file directly. Polyploid VCFs used to fail with "'R' matrix can only
  contains 0, 1, 2 or 9", because `LEA::write.geno()` only accepts diploid
  calls; ploidy up to 8 is now supported.
* **Check for Updates** looked up the BIGapp releases and compared versions as
  text. It now checks the Familia releases, compares version numbers (so
  2.10.0 is newer than 2.9.0) and shows a message when GitHub cannot be reached.
* The logo in the top bar was not shown on case-sensitive file systems (e.g. Linux servers).
* Validate Pedigree and Find Parentage no longer write `Rplots.pdf` to the
  working directory (or open a plot window) when they run; the plot is only
  shown in the app.
* Removed unused internal code and imports.
* New short vignette (`vignette("Familia")`) that explains how to launch the app and what each tab does.
* Find Parentage: the progeny input is now labeled **Progeny / Pedigree File** and its instructions state that a pedigree file is accepted.
* Find Parentage: new **Top N** input reports the best N candidates per progeny,
  and results include a `tied_candidates` count that flags ambiguous assignments.
* Find Parentage: new **Fill in pedigree** method fills missing parents of a
  pedigree. It is grayed out unless the progeny file is a 3-column pedigree
  (`id`, `male_parent`, `female_parent`); the parents file with sex M/F/A is
  still required.
* Validate Pedigree: new **Marker Summary** table (per-marker trios tested,
  mismatches and mismatch %) and `marker_summary.txt` in the download.
* Validate Pedigree: new **Marker Trio Table** (status of every marker in every trio,
  from BIGpopA `marker_trio_table`) shown in the results and saved as
  `marker_trio_table.txt` in the download.
* Validate Pedigree no longer reports suggested replacement parents (removed in BIGpopA 2.2.0); use Find Parentage > Fill in pedigree on the corrected pedigree instead.
* Updated the Help panels for the new options.
* Requires 'BIGpopA' (>= 2.2.0).

# Familia 2.1.0

* Find Parentage, Validate Pedigree and PolyBreedTools now accept genotypes as
  text files (.txt/.csv), VCF files (.vcf/.vcf.gz), or 'PLINK' .ped files, in
  addition to the previous tab-separated format. VCF calls are converted to
  allele-B dosages using the selected Ploidy; .ped files are diploid only.
* PolyBreedTools: a .map upload appears for each .ped genotype file so markers
  are matched by name. If either genotype file is a .ped, both must be; the
  validation .ped is coded with the reference panel's counted allele per marker.
* PolyBreedTools: new **Assignment threshold (%)** input. Samples whose highest
  line proportion is below the threshold are labeled "Undetermined". Changes
  apply without re-running the estimation. The default is 50; use 0 to always assign the highest line.
* SNMF: the plot, Q matrix and downloads use the best (lowest cross-entropy)
  run for the selected K by default. The manual run selector moved to an
  **Advanced Options** dialog for diagnostics; changing K or re-running resets
  to the best run. The Help panel now explains run selection.
* Updated the in-app Instructions and Help panels for the new input formats.
* Column names `id`, `male_parent`, `female_parent` and `sex` in uploaded files
  are no longer case sensitive (e.g. `ID`, `Male_Parent` are accepted).
* VCF uploads (all modules, including SNMF) now stop with a clear message when
  the selected Ploidy differs from the most common ploidy in the file, and
  suggest the ploidy to use (e.g. "The VCF looks tetraploid (ploidy 4) ...
  Set Ploidy to 4 and run again.").
* Fixed "Save Image" downloads: the file type is set explicitly, svg export works
  without the 'svglite' package, and saving errors are shown in the app.
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
