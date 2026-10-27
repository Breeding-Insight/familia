#' Internal base-package imports
#'
#' @importFrom utils capture.output packageVersion sessionInfo str write.table
#' @noRd
NULL

# vcfR is in Imports because BIGpopA::vcf_to_dosage() reads VCF uploads with it
# and BIGpopA only suggests it; the import marks it as used for R CMD check
#' @importFrom vcfR read.vcfR
#' @noRd
NULL

# Declare variables used in non-standard evaluation (ggplot2 aes() and dplyr
# verbs) so that R CMD check does not raise "no visible binding" NOTEs.
utils::globalVariables(c(
  "category",          # polybreedtools ancestry plot
  "Cluster",           # SNMF Q-matrix plot
  "ID",                # sample identifier
  "K",                 # number of ancestral clusters
  "min_cross_entropy", # SNMF cross-entropy plot
  "percent",           # polybreedtools ancestry plot
  "Q"                  # SNMF ancestry proportion
))
