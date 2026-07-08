#' Convert GT format to numeric dosage
#' @param gt a genotype matrix with samples as columns and variants as rows
#' @return numeric genotype values
#' @noRd
convert_to_dosage <- function(gt) {
  # Split the genotype string
  alleles <- strsplit(gt, "[|/]")
  # Sum the alleles; missing alleles (".", NA) make the whole call NA
  sapply(alleles, function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(sum(x))
    }
  })
}
