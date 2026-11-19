testthat::local_edition(3)

# BreedTools tab (R/mod_polybreedtools.R), run with shiny::testServer().
# Two tetraploid reference lines with fixed frequencies (LineA = 1 1 1 0 0 0,
# LineB = 0 0 1 1 1 0), so the line proportions are known exactly: VA is pure
# LineA, VB pure LineB and VM 75% LineA and 25% LineB.

bt_reference <- rbind(A1 = c(4, 4, 4, 0, 0, 0), A2 = c(4, 4, 4, 0, 0, 0),
                      B1 = c(0, 0, 4, 4, 4, 0), B2 = c(0, 0, 4, 4, 4, 0))
bt_validation <- rbind(VA = c(4, 4, 4, 0, 0, 0), VB = c(0, 0, 4, 4, 4, 0), VM = c(3, 3, 4, 1, 1, 0))
colnames(bt_reference) <- colnames(bt_validation) <- paste0("m", 1:6)

# Dosage table as the lines of a tab-separated file with an ID column
geno_lines <- function(m) {
  c(paste(c("ID", colnames(m)), collapse = "\t"),
    vapply(seq_len(nrow(m)), function(i) paste(c(rownames(m)[i], ifelse(is.na(m[i, ]), "NA", m[i, ])),
                                               collapse = "\t"), ""))
}
# The same table as a tetraploid VCF
vcf_lines <- function(m) {
  gt <- function(d) paste(c(rep("0", 4 - d), rep("1", d)), collapse = "/")
  c("##fileformat=VCFv4.3",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    paste(c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", rownames(m)), collapse = "\t"),
    vapply(seq_len(ncol(m)), function(j) paste(c("chr1", 100 * j, colnames(m)[j], "A", "G", ".", ".", ".", "GT",
                                                 vapply(m[, j], gt, "")), collapse = "\t"), ""))
}
bt_ids <- c("LineA\tLineB", "A1\tB1", "A2\tB2")
bt_expected <- data.frame(ID = c("VA", "VB", "VM"), LineA = c("100.0%", "0.0%", "75.0%"),
                          LineB = c("0.0%", "100.0%", "25.0%"),
                          `Predicted line` = c("LineA", "LineB", "LineA"), R2 = c(1, 1, 1),
                          check.names = FALSE)
bt_inputs <- utils::modifyList(breedtools_defaults, list(ploidy = 4))

test_that("the tiles count the lines, their reference samples, the samples to assign and the shared markers", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(c("LineA\tLineB", "A1\tB1", "A2\tB2", "A9\t"), "ids.txt")   # A9 is not genotyped
  shiny::testServer(mod_polybreedtools_server, {
    expect_identical(tiles(output$dashboard),
                     c("Reference lines" = DASH, "Reference samples per line" = DASH,
                       "Samples to assign" = DASH, "Shared markers" = DASH))
    session$setInputs(ref_ids_file = ids)
    expect_identical(tiles(output$dashboard),
                     c("Reference lines" = "2", "Reference samples (LineA / LineB)" = "3 / 2",
                       "Samples to assign" = DASH, "Shared markers" = DASH))
    session$setInputs(reference_file = ref)   # now counts the IDs found in the reference genotypes
    expect_identical(tiles(output$dashboard)[["Reference samples (LineA / LineB)"]], "2 / 2")
    session$setInputs(validation_file = val)
    expect_identical(tiles(output$dashboard),
                     c("Reference lines" = "2", "Reference samples (LineA / LineB)" = "2 / 2",
                       "Samples to assign" = "3", "Shared markers" = "6"))
    expect_identical(tiles_note(output$dashboard), NA_character_)
  })
})

test_that("uploading the reference, Reference IDs or validation file opens the Data Overview tab", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    log  <- record_session(session)
    tabs <- function() input_updates(log, "polybreedtools_results_tabs")
    session$flushReact()                                   # start-up, no file
    expect_length(tabs(), 0)
    session$setInputs(reference_file = ref)
    session$setInputs(ref_ids_file = ids)
    session$setInputs(validation_file = val)
    expect_identical(tabs(), rep(list(list(value = "Data Overview")), 3))
    session$setInputs(ploidy = 4, assign_threshold = 80)   # other inputs do not open it
    expect_length(tabs(), 3)
  })
})

test_that("with more than four lines the tile shows the first four, and unreadable files are noted", {
  ids <- upload_text(c("L1\tL2\tL3\tL4\tL5", "a\tb\tc\td\te"), "ids.txt")
  gz  <- upload_text("x", "reference.txt.gz")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(ref_ids_file = ids, reference_file = gz)
    expect_identical(tiles(output$dashboard)[1:2],
                     c("Reference lines" = "5", "Reference samples (L1 / L2 / L3 / L4 / ...)" = "1 / 1 / 1 / 1 / ..."))
    expect_identical(tiles_note(output$dashboard),
                     "Could not read the reference genotypes: Compressed genotype files must be VCFs (.vcf.gz).")
  })
})

test_that("Run Estimation needs the three files", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  shiny::testServer(mod_polybreedtools_server, {
    log <- record_session(session)
    session$setInputs(!!!bt_inputs, reference_file = ref, run = 1)
    expect_null(poly_items$prediction)
    expect_error(output$status, "hasn't been defined yet", fixed = TRUE)
    expect_identical(button_state(log, "download_poly_all"), NA_character_)
  })
})

test_that("the estimated proportions, predicted lines and R2 are exact for known mixtures", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    log <- record_session(session)
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)
    expect_equal(as.matrix(poly_items$prediction),
                 matrix(c(1, 0, 0.75, 0, 1, 0.25), nrow = 3,
                        dimnames = list(c("VA", "VB", "VM"), c("LineA", "LineB"))), tolerance = 1e-6)
    expect_equal(poly_items$r2, c(VA = 1, VB = 1, VM = 1), tolerance = 1e-6)
    expect_identical(plain(poly_items$pred_results), bt_expected)
    expect_identical(result_data(), poly_items$pred_results)
    expect_identical(output$status, "Estimation complete. File ready for download.")
    expect_identical(button_state(log, "download_poly_all"), "enabled")
  })
})

test_that("the assignment threshold applies at once, without running again", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)
    prediction <- poly_items$prediction
    lines <- function() unname(poly_items$pred_results[["Predicted line"]])

    session$setInputs(assign_threshold = 80)
    expect_identical(lines(), c("LineA", "LineB", "Undetermined"))
    expect_identical(poly_items$prediction, prediction)   # not estimated again
    session$setInputs(assign_threshold = 0)
    expect_identical(lines(), c("LineA", "LineB", "LineA"))
    session$setInputs(assign_threshold = NA)               # empty box: always assign
    expect_identical(lines(), c("LineA", "LineB", "LineA"))
    session$setInputs(assign_threshold = 50)
    expect_identical(lines(), c("LineA", "LineB", "LineA"))
    expect_identical(poly_items$id_order$predicted_line, c("LineA", "LineB", "LineA"))
  })
})

test_that("the Ancestry Plot groups the samples by predicted line, Undetermined last, unless unsorted", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)
    x_order <- function() ggplot2::layer_scales(ancestry_plot())$x$get_limits()
    expect_identical(x_order(), c("VA", "VM", "VB"))   # LineA (highest first), then LineB
    session$setInputs(assign_threshold = 80)
    expect_identical(x_order(), c("VA", "VB", "VM"))   # VM is Undetermined
    session$setInputs(poly_sort_by_predicted = FALSE)
    expect_identical(x_order(), c("VA", "VB", "VM"))   # file order
  })
})

test_that("the downloads give the results table and the plot", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  out_dir <- withr::local_tempdir()
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)

    zip_path <- output$download_poly_all
    xlsx     <- paste0("lineage_estimation_", Sys.Date(), ".xlsx")
    expect_identical(basename(zip_path), paste0("polybreedtools_results_", Sys.Date(), ".zip"))
    expect_identical(zip::zip_list(zip_path)$filename, xlsx)
    zip::unzip(zip_path, exdir = out_dir)
    back <- read_sheet(file.path(out_dir, xlsx), 1)
    expect_identical(names(back), names(bt_expected))
    expect_identical(back[["Predicted line"]], bt_expected[["Predicted line"]])
    expect_identical(back$LineA, bt_expected$LineA)
    expect_equal(back$R2, bt_expected$R2)

    session$setInputs(poly_image_type = "png", poly_image_width = 3, poly_image_height = 3, poly_image_res = 50)
    img <- output$download_poly_figure
    expect_identical(basename(img), paste0("polybreedtools_ancestry_plot_", Sys.Date(), ".png"))
    expect_identical(file_magic(img, 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  })
})

test_that("a Reference IDs file in long format is refused with the expected layout", {
  ref  <- upload_text(geno_lines(bt_reference), "reference.txt")
  val  <- upload_text(geno_lines(bt_validation), "validation.txt")
  long <- upload_text(c("ID,line", "A1,LineA", "A2,LineA", "B1,LineB", "B2,LineB"), "ids.csv")
  shiny::testServer(mod_polybreedtools_server, {
    log <- record_session(session)
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = long, validation_file = val)
    session$setInputs(run = 1)
    expect_identical(output$status, paste0(
      "Error during estimation: None of the IDs listed under 'line' in the Reference IDs file are in the ",
      "Reference Genotypes.\n\nThe Reference IDs file needs one column per population: the header is the ",
      "population name and the column lists its reference sample IDs (see Example Inputs). A file with one ",
      "column of sample IDs and another of population names has to be rearranged to that layout."
    ))
    expect_null(poly_items$prediction)
    expect_identical(button_state(log, "download_poly_all"), "disabled")
  })
})

test_that("Reference IDs, samples and markers that are not used are listed in the Status box", {
  ref <- upload_text(geno_lines(bt_reference), "reference.txt")
  low <- rbind(bt_validation, VX = c(NA, NA, NA, NA, 1, 1))   # 2 of 6 calls
  low[c("VA", "VB", "VM"), "m6"] <- NA                         # m6 has no call left
  val <- upload_text(geno_lines(low), "validation.txt")
  ids <- upload_text(c("LineA\tLineB", "A1\tB1", "A2\tB2", "A9\t"), "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)
    status <- output$status
    expect_match(status, "^Estimation complete\\. File ready for download\\.")
    expect_match(status, paste0("WARNING: The following Reference IDs are not in the Reference Genotypes and ",
                                "were not used:\n   • A9"), fixed = TRUE)
    expect_match(status, paste0("WARNING: The following validation samples were removed due to genotyping ",
                                "rate < 50%:\n   • VX"), fixed = TRUE)
    expect_match(status, paste0("WARNING: The following markers were removed from validation because they had ",
                                "no successful genotype calls:\n   • m6"), fixed = TRUE)
    expect_identical(plain(poly_items$pred_results), bt_expected)   # m6 adds nothing to the fit
  })
})

test_that("files that cannot be estimated give the reason in the Status box", {
  ref   <- upload_text(geno_lines(bt_reference), "reference.txt")
  ids   <- upload_text(bt_ids, "ids.txt")
  dup   <- upload_text(geno_lines(rbind(bt_validation, VA = bt_validation["VA", ])), "validation.txt")
  other <- bt_validation
  colnames(other) <- paste0("n", 1:6)
  none  <- upload_text(geno_lines(other), "validation.txt")
  empty <- upload_text(geno_lines(matrix(c(NA, NA, NA, NA, NA, 1), nrow = 1,
                                         dimnames = list("VA", paste0("m", 1:6)))), "validation.txt")
  gap   <- bt_reference
  gap[c("A1", "A2"), "m1"] <- NA
  ref_gap <- upload_text(geno_lines(gap), "reference.txt")
  val_ok  <- upload_text(geno_lines(bt_validation), "validation.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = dup)
    session$setInputs(run = 1)
    expect_identical(output$status, paste(
      "Error: The following sample IDs have duplicates in your validation file.",
      "Please check your input file and remove or rename the following IDs:\n   • VA"))

    session$setInputs(validation_file = none, run = 2)
    expect_identical(output$status,
                     "Error during estimation: No marker names are shared between the reference and validation genotypes.")

    session$setInputs(validation_file = empty, run = 3)
    expect_identical(output$status,
                     "Error during estimation: No validation samples remain after filtering for genotyping rate >= 50%.")

    session$setInputs(reference_file = ref_gap, validation_file = val_ok, run = 4)
    expect_identical(output$status, paste0(
      "Error: The following markers were not successfully genotyped for at least one reference population, ",
      "please remove or correct them:\n  • LineA: m1\n\nPlease remove or correct these markers"))
    expect_null(poly_items$prediction)
  })
})

test_that("a VCF reference gives the same estimate as the dosage table, and its ploidy is checked", {
  vcf <- upload_text(vcf_lines(bt_reference), "reference.vcf")
  val <- upload_text(geno_lines(bt_validation), "validation.txt")
  ids <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = vcf, ref_ids_file = ids, validation_file = val)
    session$setInputs(run = 1)
    expect_identical(plain(poly_items$pred_results), bt_expected)

    session$setInputs(ploidy = 2, run = 2)
    expect_identical(output$status, paste(
      "Error during estimation: The VCF looks tetraploid (ploidy 4): most genotype calls have 4 alleles,",
      "but Ploidy is set to 2. Set Ploidy to 4 and run again."))
  })
})

test_that("PLINK .ped files need their .map, and a validation .ped is coded like the reference .ped", {
  ref_ped <- upload_text(c("FAM A1 0 0 0 -9 G G G G A A A A", "FAM A2 0 0 0 -9 G G G G A A A A",
                           "FAM B1 0 0 0 -9 A A A A G G G G", "FAM B2 0 0 0 -9 A A A A G G G G"), "reference.ped")
  # only VA: coded on its own, A would be counted at m3 and m4 (the only allele seen)
  val_ped <- upload_text("FAM VA 0 0 0 -9 G G G G A A A A", "validation.ped")
  map     <- c("1\tm1\t0\t100", "1\tm2\t0\t200", "1\tm3\t0\t300", "1\tm4\t0\t400")
  ref_map <- upload_text(map, "reference.map")
  val_map <- upload_text(map, "validation.map")
  val_txt <- upload_text(geno_lines(matrix(c(2, 2, 0, 0), nrow = 1, dimnames = list("VA", paste0("m", 1:4)))),
                         "validation.txt")
  ids     <- upload_text(bt_ids, "ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!breedtools_defaults, ref_ids_file = ids, reference_file = ref_ped)
    expect_match(ui_html(output$reference_map_ui), paste0('id="', session$ns("reference_map"), '"'), fixed = TRUE)

    session$setInputs(validation_file = val_txt, run = 1)
    expect_identical(output$status, paste("Error during estimation: When using a PLINK .ped file, both the",
                                          "reference and validation genotypes must be .ped files."))
    expect_null(output$validation_map_ui)

    session$setInputs(validation_file = val_ped, run = 2)
    expect_identical(output$status, "Error during estimation: Upload the .map file for the reference .ped.")
    expect_match(ui_html(output$validation_map_ui), paste0('id="', session$ns("validation_map"), '"'), fixed = TRUE)

    session$setInputs(reference_map = ref_map)
    session$setInputs(run = 3)
    expect_identical(output$status, "Error during estimation: Upload the .map file for the validation .ped.")

    session$setInputs(validation_map = val_map)
    session$setInputs(run = 4)
    expect_identical(plain(poly_items$pred_results),
                     data.frame(ID = "VA", LineA = "100.0%", LineB = "0.0%", `Predicted line` = "LineA", R2 = 1,
                                check.names = FALSE))

    # a new reference .ped needs its .map again
    session$setInputs(reference_file = ref_ped)
    expect_null(ped_maps$reference)
  })
})

test_that("the example files give the predicted lines they were simulated with", {
  ref <- upload_example("example_breedtools_reference.txt")
  val <- upload_example("example_breedtools_validation.txt")
  ids <- upload_example("example_breedtools_ref_ids.txt")
  shiny::testServer(mod_polybreedtools_server, {
    session$setInputs(!!!bt_inputs, reference_file = ref, ref_ids_file = ids, validation_file = val)
    expect_identical(tiles(output$dashboard),
                     c("Reference lines" = "3", "Reference samples (LineA / LineB / LineC)" = "20 / 20 / 20",
                       "Samples to assign" = "13", "Shared markers" = "300"))
    session$setInputs(run = 1)
    expect_identical(output$status, "Estimation complete. File ready for download.")
    pred <- poly_items$pred_results
    expect_identical(names(pred), c("ID", "LineA", "LineB", "LineC", "Predicted line", "R2"))
    expect_identical(unname(pred[["Predicted line"]][match(sprintf("V%02d", 1:12), pred$ID)]),
                     c(rep("LineA", 3), rep("LineB", 3), rep("LineC", 3), "LineA", "Undetermined", "Undetermined"))
    r2 <- poly_items$r2
    expect_true(r2[["V13"]] < 0.8 * min(r2[names(r2) != "V13"]))   # V13 is from a line missing in the panel
  })
})

test_that("the Example Inputs box shows and downloads the example files, and Help opens", {
  shiny::testServer(mod_polybreedtools_server, {
    log <- record_session(session)
    expect_match(output$example_ids, "LineA", fixed = TRUE)
    expect_match(output$example_ids, "RA01", fixed = TRUE)
    expect_match(output$example_genos, "RA01", fixed = TRUE)

    expect_identical(basename(output$download_ids), "example_breedtools_ref_ids.txt")
    expect_identical(readLines(output$download_ids), readLines(extdata("example_breedtools_ref_ids.txt")))
    expect_identical(basename(output$download_genos), "breedtools_example_genotypes.zip")
    expect_identical(zip::zip_list(output$download_genos)$filename,
                     c("example_breedtools_reference.txt", "example_breedtools_validation.txt"))
    expect_identical(basename(output$download_examples), "breedtools_examples.zip")
    expect_identical(zip::zip_list(output$download_examples)$filename, example_files$polybreedtools)

    session$setInputs(help_btn = 1)
    expect_match(log$modals, "PolyBreedTools - Help", fixed = TRUE)
    expect_match(log$modals, 'id="modal_pbt_help_ref_ids"', fixed = TRUE)
  })
})
