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
