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
