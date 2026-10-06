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
    # Infer the ploidy from the GT calls and stop if it differs from the selection
    vcf <- BIGpopA::vcf_to_dosage(file_input$datapath, ploidy = NULL, verbose = FALSE)
    check_vcf_ploidy(attr(vcf, "ploidy"), ploidy)
    geno <- as.data.frame(vcf, check.names = FALSE)
    names(geno)[names(geno) == "id"] <- id_name
    return(geno)
  }
  if (grepl("\\.gz$", name)) {
    stop("Compressed genotype files must be VCFs (.vcf.gz).")
  }
  geno <- if (tools::file_ext(name) == "csv") {
    utils::read.csv(file_input$datapath, header = TRUE,
                    stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.table(file_input$datapath, header = TRUE, sep = "\t",
                      stringsAsFactors = FALSE, check.names = FALSE,
                      comment.char = "", quote = "")
  }

  # ID column name is not case sensitive (id, ID, Id): rename it to id_name
  if (!id_name %in% names(geno)) {
    hit <- which(tolower(trimws(names(geno))) == "id")
    if (length(hit) >= 1) names(geno)[hit[1]] <- id_name
  }
  geno
}

#' Describe a ploidy level for messages, e.g. "tetraploid (ploidy 4)"
#' @noRd
ploidy_label <- function(p) {
  lbl <- c(`2` = "diploid", `3` = "triploid", `4` = "tetraploid",
           `5` = "pentaploid", `6` = "hexaploid", `8` = "octoploid")
  key <- as.character(p)
  if (key %in% names(lbl)) paste0(lbl[[key]], " (ploidy ", p, ")") else paste("ploidy", p)
}

#' Most common ploidy (allele slots per call) in a VCF GT matrix
#' @noRd
gt_ploidy <- function(gt) {
  calls <- as.vector(gt)
  calls <- calls[!is.na(calls) & nzchar(calls)]
  if (length(calls) == 0) return(NA_integer_)
  slots <- lengths(strsplit(calls, "[/|]"))
  as.integer(names(which.max(table(slots))))
}

#' Stop when the selected ploidy differs from the VCF's most common ploidy
#'
#' @param vcf_ploidy Ploidy observed in the VCF (most common call length).
#' @param ploidy Ploidy selected in the app.
#' @noRd
check_vcf_ploidy <- function(vcf_ploidy, ploidy) {
  if (length(vcf_ploidy) == 1 && !is.na(vcf_ploidy) && as.integer(ploidy) != vcf_ploidy) {
    stop("The VCF looks ", ploidy_label(vcf_ploidy), ": most genotype calls have ",
         vcf_ploidy, " alleles, but Ploidy is set to ", ploidy,
         ". Set Ploidy to ", vcf_ploidy, " and run again.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Save a ggplot for a "Save Image" download button
#'
#' Shared by every module's figure download. The graphics device is set from
#' the chosen file type instead of guessed from the temporary file name, and
#' svg uses 'svglite' when installed or the built-in grDevices::svg otherwise.
#' Errors are shown to the user as a notification instead of failing silently.
#'
#' @param plot ggplot object.
#' @param file Path supplied by shiny::downloadHandler().
#' @param ext File type: "png", "jpeg", "tiff", "pdf" or "svg".
#' @param width,height Size in inches.
#' @param dpi Resolution for raster formats.
#' @noRd
save_plot_file <- function(plot, file, ext, width = 8, height = 5, dpi = 300) {
  device <- switch(
    ext,
    png  = "png",
    jpeg = "jpeg",
    tiff = "tiff",
    pdf  = "pdf",
    svg  = if (requireNamespace("svglite", quietly = TRUE)) "svg" else grDevices::svg,
    stop("Unsupported image type: ", ext)
  )
  tryCatch(
    ggplot2::ggsave(filename = file, plot = plot, device = device,
                    width = width, height = height, units = "in", dpi = dpi),
    error = function(e) {
      shiny::showNotification(paste("Could not save the image:", conditionMessage(e)),
                              type = "error", duration = 10)
      stop(e)
    }
  )
  invisible(file)
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
