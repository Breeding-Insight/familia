[![CRAN status](https://www.r-pkg.org/badges/version/Familia)](https://CRAN.R-project.org/package=Familia)
[![R-CMD-check](https://github.com/Breeding-Insight/Familia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Breeding-Insight/Familia/actions/workflows/R-CMD-check.yaml)
[![R](https://img.shields.io/badge/R-%3E%3D%204.4-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Web%20Application-blueviolet)](https://shiny.posit.co/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![GitHub issues](https://img.shields.io/github/issues/Breeding-Insight/Familia)](https://github.com/Breeding-Insight/Familia/issues)
[![GitHub pull requests](https://img.shields.io/github/issues-pr/Breeding-Insight/Familia)](https://github.com/Breeding-Insight/Familia/pulls)
[![GitHub Release](https://img.shields.io/github/v/release/Breeding-Insight/Familia?include_prereleases)](https://github.com/Breeding-Insight/Familia/releases/latest)

<div align="center">
<img width="250" height="250" alt="Familia_logo" src="https://github.com/user-attachments/assets/600951c0-65e5-4761-93a6-56d4c9a50ad0"/>
</div>

### Pedigree Validation and Ancestry Assessment App

Familia is a Shiny web application developed by [Breeding Insight](https://breedinginsight.org/) 
to support pedigree validation and ancestry assessment of plant and animal
populations. The app integrates Mendelian error analysis, parentage assignment,
supervised ancestry estimation, and unsupervised ancestry inference to help
breeding programs evaluate genomic relationships through an accessible,
providing a web-based interface to [BIGpopA](https://github.com/Breeding-Insight/BIGpopA)

## Overview

Accurate pedigree records and ancestry information are foundational to
modern breeding programs. Familia provides an interactive and reproducible
framework for:

- Detecting and correcting structural pedigree errors before downstream analysis
- Validating pedigree trios using Mendelian error analysis
- Assigning parentage to progeny from candidate parent pools
- Estimating line and breed composition through supervised ancestry methods
- Inferring population structure through unsupervised ancestry estimation

The application is designed to be species-agnostic and adaptable to a wide
range of plant and animal breeding programs.

---

## Key Features

### Pedigree Cleaning
- Detection of exact duplicate records, conflicting trios, and inconsistent sex roles
- Automatic addition of missing parents with unknown parent codes
- Detection of cycles and circular dependencies in pedigree relationships
- Configurable correction options with interactive review of flagged records
- Exportable corrected pedigree and per-issue result tables

### Pedigree Validation
- Mendelian error analysis across trios using marker genotype data
- Configurable error thresholds for trio and single-parent evaluations
- Automatic classification of trios into Pass, Fail, Low Markers,
  No Genotype Data, Founders, and Missing Parents categories
- Optional founders file to preserve known founder trios
- Configurable ploidy: even ploidy uses the full Mendelian test, odd ploidy (e.g. triploid) a homozygosity-based check
- Exportable corrected pedigree and per-status result tables

### Parentage Assignment
- Support for best pair, best male parent, best female parent,
  and best match assignment methods
- Configurable error threshold and minimum marker filters
- Tie detection and self-match exclusion options
- Configurable ploidy for diploid and polyploid data
- Results classified as Pass, High Error, or Low Markers
- Exportable full results and per-status tables

### Line/Breed Composition Estimation (PolyBreedTools)
- Supervised ancestry estimation based on reference population genotypes
- Support for polyploid species via configurable ploidy parameter
- Interactive ancestry bar plot with customizable color palettes
- Automatic filtering of low-quality samples and markers
- Exportable results as Excel files

### Unsupervised Ancestry Estimation (SNMF)
- Unsupervised ancestry inference via LEA::snmf()
- Supports VCF, VCF.gz, and LEA .geno input formats
- Configurable K range, repetitions, alpha, iterations, and tolerance
- Cross-entropy-based automatic or manual K selection
- Interactive Q-matrix ancestry plot with sort and label controls
- Exportable Q-matrix CSV and cross-entropy summary

---

## Installation and Running the App

Familia uses a golem application structure and is installed like a standard R
package.

### Install from CRAN

```r
install.packages("Familia")
```

### Install the development version from GitHub

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("Breeding-Insight/Familia")
```

### Run Familia
```r
Familia::run_app()
```

### Dependencies
Key R packages used by Familia include:

* shiny
* BIGpopA
* bs4Dash
* DT
* vcfR
* data.table
* rlang
* openxlsx
* zip
* LEA (required for SNMF-based ancestry inference)
---
## Citation

If you use Familia in research, please cite it as:
Chinchilla-Vargas, J., Sandercock, A. M., & Breeding Insight Team (2026). Familia: 'shiny' Application for Population Structure and Ancestry Assessments. R package version 2.0.0. https://CRAN.R-project.org/package=Familia

You can obtain this citation (including a BibTeX entry) at any time by running `citation("Familia")` in R.

#### Also cite:
* **For sNMF:**
    - [Frichot et.al](https://pmc.ncbi.nlm.nih.gov/articles/pmid/24496008/) for original methods
    - [Frichot et.al](https://doi.org/10.1111/2041-210X.12382) for LEA package
* **BreedTools<sup>poly</sup>:**
    - [Funkhouser et al.](https://doi.org/10.2527/tas2016.0003) for original BreedTools methods  
    - [Sandercock et al.](https://doi.org/10.1002/tpg2.70067) for methods expansion to polyploidy  

## License
Familia is released under the Apache License, Version 2.0.
See the LICENSE file or https://www.apache.org/licenses/LICENSE-2.0 for details.

# Acknowledgments
Familia is developed as part of the Breeding Insight initiative
(https://www.breedinginsight.org) to provide open-source, data-driven tools
for modern breeding programs.
