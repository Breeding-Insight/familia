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

#' Write an uploaded genotype file as a LEA .geno file (SNMF)
#'
#' Reads the upload with read_genotype_upload(), the reader used by the other
#' tabs (a VCF is converted with BIGpopA::vcf_to_dosage() and stops when the
#' Ploidy does not match it), then writes the LEA .geno format: one line per
#' marker and one digit per sample (0 to ploidy, 9 = missing).
#' LEA::write.geno() is not used because it only accepts 0, 1, 2 and 9.
#'
#' @param file_input A Shiny fileInput value (list with name and datapath).
#' @param geno_path Path of the .geno file to write.
#' @param ploidy Integer ploidy, up to 8 (one digit per call).
#' @return A list with geno_path and sample_ids.
#' @noRd
write_geno_upload <- function(file_input, geno_path, ploidy) {
  ploidy <- as.integer(ploidy)
  if (ploidy > 8) {
    stop("SNMF supports ploidy up to 8 (one digit per call in the .geno file).", call. = FALSE)
  }
  geno <- read_genotype_upload(file_input, ploidy = ploidy)
  dose <- as.matrix(geno[, names(geno) != "id", drop = FALSE])   # samples x markers
  if (nrow(dose) == 0 || ncol(dose) == 0) {
    stop("No genotype calls were found in the uploaded file.", call. = FALSE)
  }
  dose[is.na(dose) | dose < 0 | dose > ploidy] <- 9
  storage.mode(dose) <- "integer"
  writeLines(apply(dose, 2, paste, collapse = ""), geno_path)   # one line per marker
  list(geno_path = geno_path, sample_ids = as.character(geno$id))
}

#' Describe a ploidy level for messages, e.g. "tetraploid (ploidy 4)"
#' @noRd
ploidy_label <- function(p) {
  lbl <- c(`2` = "diploid", `3` = "triploid", `4` = "tetraploid",
           `5` = "pentaploid", `6` = "hexaploid", `8` = "octoploid")
  key <- as.character(p)
  if (key %in% names(lbl)) paste0(lbl[[key]], " (ploidy ", p, ")") else paste("ploidy", p)
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

#' Example input files of each tab (inst/extdata)
#'
#' Simulated by data-raw/make_example_data.R and used in the user guide.
#' @noRd
example_files <- list(
  ped_cleaner    = c("example_pedigree.txt"),
  validate_ped   = c("example_pedigree_cleaned.txt", "example_genotypes.txt",
                     "example_founders.txt", "example_genotypes.vcf",
                     "example_genotypes.ped", "example_genotypes.map"),
  find_parentage = c("example_genotypes.txt", "example_parents.txt",
                     "example_progeny.txt", "example_pedigree_validated.txt",
                     "example_founders.txt"),
  polybreedtools = c("example_breedtools_reference.txt", "example_breedtools_ref_ids.txt",
                     "example_breedtools_validation.txt"),
  snmf           = c("example_snmf.vcf")
)

#' Download handler for example files: one file as is, several as a zip
#'
#' @param files File names in inst/extdata.
#' @param zip_name Name of the zip file when there are several files.
#' @noRd
example_download_handler <- function(files, zip_name = "familia_example_files.zip") {
  shiny::downloadHandler(
    filename = function() if (length(files) == 1) files else zip_name,
    content  = function(file) {
      paths <- vapply(files, function(f) app_sys("extdata", f), character(1))
      if (!all(nzchar(paths))) {
        shiny::showNotification("Example files not found. Reinstall Familia.",
                                type = "error", duration = 10)
        stop("Example files not found: ", paste(files[!nzchar(paths)], collapse = ", "))
      }
      if (length(files) == 1) {
        file.copy(paths, file, overwrite = TRUE)
      } else {
        zip::zip(zipfile = file, files = files, root = dirname(paths[1]))
      }
    },
    contentType = if (length(files) == 1) NA else "application/zip"
  )
}

#' "Example files" link for the Inputs box of a tab
#'
#' Pair with output$download_examples <- example_download_handler(files).
#' @param ns Module namespace function.
#' @param files File names in inst/extdata (listed when hovering the link).
#' @noRd
example_files_ui <- function(ns, files) {
  label <- if (length(files) == 1) "Example file" else "Example files (.zip)"
  shiny::div(
    style = "font-size: 12px; margin-bottom: 12px;",
    shiny::downloadLink(ns("download_examples"),
                        shiny::tagList(shiny::icon("file-arrow-down"), label),
                        title = paste(files, collapse = ", "))
  )
}

#' First rows and columns of an example file, or NULL if it is missing
#' @noRd
read_example_table <- function(file, n_rows = 5, n_cols = 6) {
  path <- app_sys("extdata", file)
  if (!nzchar(path)) return(NULL)   # read.delim("") would wait for console input
  tryCatch({
    x <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
    utils::head(x[, seq_len(min(n_cols, ncol(x))), drop = FALSE], n_rows)
  }, error = function(e) NULL)
}
