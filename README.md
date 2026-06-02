# _familia_ — Pedigree Validation and Ancestry Assessment App

[![R](https://img.shields.io/badge/R-%3E%3D%204.4-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Web%20Application-blueviolet)](https://shiny.posit.co/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![GitHub issues](https://img.shields.io/github/issues/Breeding-Insight/familia)](https://github.com/Breeding-Insight/familia/issues)
[![GitHub pull requests](https://img.shields.io/github/issues-pr/Breeding-Insight/familia)](https://github.com/Breeding-Insight/familia/pulls)
[![GitHub Release](https://img.shields.io/github/v/release/Breeding-Insight/familia?include_prereleases)](https://github.com/Breeding-Insight/familia/releases/latest)

_familia_ is a Shiny web application developed by the Breeding Insight team
to support pedigree validation and ancestry assessment of plant and animal
populations. The app integrates Mendelian error analysis, parentage assignment,
supervised ancestry estimation, and unsupervised ancestry inference to help
breeding programs evaluate genomic relationships through an accessible,
web-based interface without requiring command-line tools.

This repository follows a golem-based application structure.

---

## Overview

Accurate pedigree records and ancestry information are foundational to
modern breeding programs. _familia_ provides an interactive and reproducible
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
- Exportable corrected pedigree and per-status result tables

### Parentage Assignment
- Support for best pair, best male parent, best female parent,
  and best match assignment methods
- Configurable error threshold and minimum marker filters
- Tie detection and self-match exclusion options
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

_familia_ uses a golem application structure, allowing it to be installed
like a standard R package.

### Install from GitHub

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("Breeding-Insight/familia")
```

### Run familia
```r
familia::run_app()
```

### Dependencies
Key R packages used by _familia_ include:

* shiny
* BIGr
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
If you use _familia_ in research, please cite it as:

* Chinchilla-Vargas, J., & Sandercock, A. M.
familia: Pedigree Validation and Ancestry Assessment Shiny App.
RRID:

### Also Cite for the methods used:

* **For sNMF:** Frichot E, Mathieu F, Trouillon T, Bouchard G, Francois O. (2014). Fast and Efficient Estimation of Individual Ancestry Coefficients. Genetics, 194(4): 973--983.
* BreedTools

RRID: [PENDING]

## Contributing
Contributions are welcome. Please:

Follow existing coding and naming conventions
Clearly document new functions and modules
Test changes before submitting pull requests
Submit issues and pull requests via GitHub.

## License
_familia_ is released under the Apache License, Version 2.0.
See the LICENSE file or https://www.apache.org/licenses/LICENSE-2.0 for details.

# Acknowledgments
_familia_ is developed as part of the Breeding Insight initiative
(https://www.breedinginsight.org) to provide open-source, data-driven tools
for modern breeding programs.
```
