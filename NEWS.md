# Familia 2.3.0

This version was developed with AI assistance; all code has been reviewed by
a human.

Changes from user testing feedback.

* Pedigree Cleaner: in the Conflicting Trios table, check the **Keep** box of the
  row to keep (at most one per id) and click **Apply selection**: the check runs
  again without the other rows of that id, so the kept parents are used for the
  missing parents, sex roles and cycles. **Reset** clears the choices, and the
  Run Summary counts the kept and set-to-missing ids.
* Pedigree Cleaner, Validate Pedigree and Find Parentage: **Download Results**
  gives one Excel file instead of a zip of text files. Its first sheet (All
  Records, All Trios or All Progeny) has every record with an `issue` column
  (and, in the Cleaner, the `action` taken); one sheet per table and a
  Settings sheet with the files and options of the run follow. The pedigree
  for the next tab is a separate download: `pedigree_cleaned.txt` and
  `pedigree_validated.txt` (it was `corrected_pedigree.txt` in both).
* Every tab has a **Data Overview** tab, next to Instructions, with summary
  tiles that count what was uploaded (records, complete trios, genotyped
  individuals, markers, reference samples per line, detected ploidy, missing
  calls...) before anything is run. It opens when a file is uploaded. Each file
  is read once, on upload, and reused for the run.
* Validate Pedigree and Find Parentage: hovering over a bar of the Mendelian
  Error Plot gives its count, and clicking it lists its trios or progeny under
  the plot.
* Wording: the options are now "Set conflicting parents to missing (0)" and
  "Set parents with inconsistent sex roles to missing (0)", since the app sets
  parents it cannot trust to missing rather than correcting them. The Validate
  tables show the recommended action in plain language (e.g. "Set male parent
  to missing"); the downloads keep BIGpopA's codes. Table titles no longer
  include the action ("Exact Duplicates", "Conflicting Trios", "Missing
  Parents", "Cycles / Dependencies").
* Tables: a filter box under each column header searches one column, matches
  are highlighted, and rows are more compact. The Help panels explain that the
  Search box looks in every column, ignoring case.
* Plots show a spinner while they are drawn.
* Spaces at the start or end of a cell of an uploaded table are removed, so an
  id typed as `"A1 "` is the same individual as `"A1"`. A trailing space used
  to hide conflicting trios and duplicates in the Pedigree Cleaner.
* BreedTools: the Reference IDs file can also be a `.csv`. When none of the IDs
  of a line are in the reference genotypes, the run stops with a message naming
  the line and the expected layout (one column per line), instead of listing
  every marker as not genotyped; IDs that are not found are listed in the
  Status box. Long messages in the Status box wrap.
* SNMF: a banner explains how to install LEA from Bioconductor when it is
  missing, and the Instructions say that the example file is tetraploid
  (Ploidy 4) and that Ploidy must match your own file.
* Validate Pedigree Help: the Fail and Low Markers panels showed `<code>` tags
  as text.

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
