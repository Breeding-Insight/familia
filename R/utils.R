#' File extensions accepted by genotype upload inputs
#' @noRd
genotype_upload_accept <- c(".txt", ".tsv", ".csv", ".vcf", ".gz", ".ped")

#' Detect the format of an uploaded genotype file from its original name
#' @param file_input A Shiny fileInput value.
#' @return One of "vcf", "ped", or "text".
#' @noRd
genotype_upload_format <- function(file_input) {
  name <- tolower(file_input$name)
  if (grepl("\\.vcf(\\.gz)?$", name)) return("vcf")
  if (grepl("\\.ped$", name))         return("ped")
  "text"
}

#' Read an uploaded genotype file (text, VCF or PLINK .ped)
#'
#' Shared by the parentage, pedigree validation and PolyBreedTools modules.
#' The format is detected from the original file name, because Shiny renames
#' uploads (a .vcf.gz arrives as e.g. "0.gz"). VCF files are converted to
#' allele-B dosages with BIGpopA::vcf_to_dosage(), PLINK .ped files with
#' BIGpopA::ped_to_dosage() (markers named by position, no .map needed);
#' text files are read as CSV or tab-separated tables.
#'
#' @param file_input A Shiny fileInput value (list with name and datapath).
#' @param ploidy Integer ploidy passed to vcf_to_dosage(); must be 2 for .ped.
#' @param id_name Name to give the sample ID column of a converted file.
#' @param counted_allele Named character vector passed to ped_to_dosage() so a
#'   .ped file is coded like a previous one (e.g. validation like reference).
#' @param map_input Optional Shiny fileInput value for a PLINK .map file. When
#'   given, .ped markers are named from the .map; otherwise by position.
#' @return A data.frame with an ID column followed by marker columns. For .ped
#'   files the "counted_allele" attribute holds the allele counted per marker.
#' @noRd
read_genotype_upload <- function(file_input, ploidy, id_name = "id",
                                 counted_allele = NULL, map_input = NULL) {
  name   <- tolower(file_input$name)
  format <- genotype_upload_format(file_input)

  if (format == "ped") {
    if (as.integer(ploidy) != 2) {
      stop("PLINK .ped files are diploid; set Ploidy to 2.")
    }
    map_path <- if (!is.null(map_input)) map_input$datapath else NULL
    ped      <- BIGpopA::ped_to_dosage(file_input$datapath, map = map_path,
                                       counted_allele = counted_allele, verbose = FALSE)
    counted <- attr(ped, "counted_allele")
    geno    <- as.data.frame(ped, check.names = FALSE)
    names(geno)[names(geno) == "id"] <- id_name
    attr(geno, "counted_allele") <- counted
    return(geno)
  }

  if (format == "vcf") {
    geno <- as.data.frame(
      BIGpopA::vcf_to_dosage(file_input$datapath, ploidy = as.integer(ploidy), verbose = FALSE),
      check.names = FALSE
    )
    names(geno)[names(geno) == "id"] <- id_name
    return(geno)
  }
  if (grepl("\\.gz$", name)) {
    stop("Compressed genotype files must be VCFs (.vcf.gz).")
  }
  if (tools::file_ext(name) == "csv") {
    return(utils::read.csv(file_input$datapath, header = TRUE,
                           stringsAsFactors = FALSE, check.names = FALSE))
  }
  utils::read.table(file_input$datapath, header = TRUE, sep = "\t",
                    stringsAsFactors = FALSE, check.names = FALSE,
                    comment.char = "", quote = "")
}

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
