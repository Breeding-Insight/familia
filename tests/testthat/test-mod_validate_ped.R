testthat::local_edition(3)

# Validate Pedigree tab (R/mod_validate_ped.R), run with shiny::testServer() on
# the example files: the cleaned pedigree, its genotypes and the founders. The
# trios with deliberate errors are listed in data-raw/make_example_data.R.

validate_args <- list(parent_session = NULL)

# BIGpopA::validate_pedigree() called the way the tab calls it
validate_direct <- function(ped, geno, founders = NULL) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  BIGpopA::validate_pedigree(
    pedigree_file                 = read_table_upload(ped),
    genotypes_file                = read_genotype_upload(geno, ploidy = NULL),
    founders_file                 = if (is.null(founders)) NULL else founders$datapath,
    trio_error_threshold          = 5,
    min_markers                   = 10L,
    single_parent_error_threshold = 2,
    ploidy                        = 2L,
    verbose                       = FALSE,
    plot_results                  = TRUE,
    marker_trio_table             = TRUE
  )
}
report_tables <- c("pass", "fail", "low_markers", "no_genotype_data", "founders", "missing_parents",
                   "full_results", "corrected_pedigree", "marker_summary", "marker_trio_table")
tile_labels <- c("Records", "Unique IDs", "Complete trios", "One parent known", "Both parents unknown",
                 "Genotyped individuals", "Markers", "Testable trios")

# Center of the bar of a histogram that counts the most rows, and its limits
fullest_bar <- function(plot) {
  ld    <- ggplot2::layer_data(plot)
  bins  <- unique(ld[, c("xmin", "xmax")])
  total <- vapply(seq_len(nrow(bins)), function(i) sum(ld$count[ld$xmin == bins$xmin[i]]), numeric(1))
  i     <- which.max(total)
  list(x = (bins$xmin[i] + bins$xmax[i]) / 2, lo = bins$xmin[i], hi = bins$xmax[i], n = as.integer(total[i]),
       label = sprintf("%s to %s%%", format(max(0, bins$xmin[i])), format(bins$xmax[i])))
}

test_that("the tiles count the pedigree, the genotypes and the trios that can be tested", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    expect_identical(tiles(output$dashboard), stats::setNames(rep(DASH, 8), tile_labels))
    session$setInputs(genotypes_file = geno)
    expect_identical(tiles(output$dashboard),
                     stats::setNames(c(rep(DASH, 5), "45", "300", DASH), tile_labels))
    session$setInputs(pedigree_file = ped)
    expect_identical(tiles(output$dashboard),
                     stats::setNames(c("48", "48", "37", "3", "8", "45", "300", "35"), tile_labels))
    expect_identical(tiles_note(output$dashboard), NA_character_)
  })
})

test_that("uploading the pedigree or the genotypes opens the Data Overview tab", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  fnd  <- upload_example("example_founders.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log  <- record_session(session)
    tabs <- function() input_updates(log, "validate_results_tabs")
    session$flushReact()                                  # start-up, no file
    expect_length(tabs(), 0)
    session$setInputs(pedigree_file = ped)
    expect_identical(tabs(), list(list(value = "Data Overview")))
    session$setInputs(genotypes_file = geno)
    expect_identical(tabs(), rep(list(list(value = "Data Overview")), 2))
    session$setInputs(founders_file = fnd, ploidy = 4)    # not counted in the overview
    expect_length(tabs(), 2)
  })
})

test_that("files that cannot be used are named in the note under the tiles", {
  ped  <- upload_text(c("id\tsire\tdam", "A\t0\t0"), "pedigree.txt")
  geno <- upload_text("x", "genotypes.txt.gz")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    session$setInputs(pedigree_file = ped, genotypes_file = geno)
    expect_identical(tiles(output$dashboard), stats::setNames(rep(DASH, 8), tile_labels))
    expect_identical(tiles_note(output$dashboard),
                     paste("The pedigree file needs id, male_parent and female_parent columns.",
                           "Could not read the genotypes file: Compressed genotype files must be VCFs (.vcf.gz)."))
  })
})

test_that("Run Validation needs a pedigree and a genotypes file", {
  ped <- upload_example("example_pedigree_cleaned.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log <- record_session(session)
    session$setInputs(!!!validate_defaults, pedigree_file = ped, run_validate = 1)
    expect_null(validate_results())
    expect_length(progress_updates(log), 0)
  })
})

test_that("a run on the example files finds every planted case", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  fnd  <- upload_example("example_founders.txt")
  direct <- validate_direct(ped, geno, fnd)
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log <- record_session(session)
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno, founders_file = fnd)
    session$setInputs(run_validate = 1)
    report <- validate_results()

    for (nm in report_tables) {
      expect_identical(plain(report[[nm]]), plain(direct[[nm]]), label = nm)
    }
    expect_true(inherits(report$plot, "ggplot"))

    fr     <- plain(report$full_results)
    status <- stats::setNames(fr$status, fr$id)
    corr   <- stats::setNames(fr$recommended_correction, fr$id)
    expect_identical(sort(fr$id), sort(read_table_upload(ped)$id))   # one row per pedigree row
    expect_true(all(status[c("C01", "C02", "C03", sprintf("C%02d", 12:38))] == "pass"))
    expect_identical(unname(status[c("C04", "C05", "C06", "C11", "C08")]),
                     c("fail", "fail", "fail", "fail", "low_markers"))
    expect_identical(unname(corr[c("C04", "C05", "C06", "C11", "C08")]),
                     c("remove_male_parent", "remove_female_parent", "remove_both", "unresolved",
                       "low_markers_keep_both"))
    expect_identical(unname(status[c("C07", "C09", "C10")]),
                     c("missing_male_parent", "missing_male_parent", "missing_female_parent"))
    expect_identical(unname(status[c("M1", "M1c", "M2", "M3", "F1", "F2", "F3")]), rep("founders", 7))
    expect_identical(unname(status[c("M4", "X1", "X2")]), rep("no_genotype_data", 3))
    expect_setequal(utils::head(report$marker_summary$marker, 3), c("snp050", "snp150", "snp250"))

    expect_identical(run_settings(), list(
      pedigree_file = "example_pedigree_cleaned.txt", genotypes_file = "example_genotypes.txt",
      founders_file = "example_founders.txt", trio_threshold = 5, single_threshold = 2,
      min_markers = 10L, ploidy = 2L
    ))
    expect_identical(progress_titles(log),
                     c("Reading input files...", "Evaluating trios...", "Compiling results...", "Finished"))
    expect_identical(last_progress(log)$status, "success")
    expect_identical(button_state(log, "download_validate_all"), "enabled")
    expect_identical(button_state(log, "download_pedigree"),     "enabled")
    expect_identical(input_updates(log, "validate_results_tabs"),
                     list(list(value = "Data Overview"), list(value = "Issue Tables")))   # uploads, then run
  })
})

test_that("the Run Summary and the Issue Tables count each result category", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  fnd  <- upload_example("example_founders.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno, founders_file = fnd)
    session$setInputs(run_validate = 1)

    expect_identical(banner_headline(output$summary_banner), "5 trio(s) flagged. Review the Issue Tables tab.")
    expect_identical(banner_counts(output$summary_banner),
                     c("Total trios" = "48", "Pass" = "30", "Fail" = "4", "Low markers" = "1",
                       "No genotype data" = "3", "Founders" = "7", "Missing parents" = "3"))
    expect_match(ui_html(output$summary_banner), "background-color:#fff3cd", fixed = TRUE)

    ui <- output$results_ui
    expect_identical(section_counts(ui),
                     c("Pass" = "30 record(s) found", "Fail" = "4 record(s) found",
                       "Low Markers" = "1 record(s) found", "No Genotype Data" = "3 record(s) found",
                       "Founders" = "7 record(s) found", "Missing Parents" = "3 record(s) found",
                       "Marker Summary" = "300 record(s) found", "Marker Trio Table" = "300 record(s) found"))
    for (tbl in c("tbl_pass", "tbl_fail", "tbl_low_markers", "tbl_no_genotype_data", "tbl_founders",
                  "tbl_missing_parents", "tbl_marker_summary", "tbl_marker_trio_table")) {
      expect_match(ui_html(ui), paste0('id="', session$ns(tbl), '"'), fixed = TRUE)
    }
  })
})

test_that("without a founders file the founders are trios with both parents missing", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno)
    session$setInputs(run_validate = 1)
    fr <- plain(validate_results()$full_results)
    expect_identical(fr$status[match(c("M1", "M1c", "M2", "M3", "F1", "F2", "F3"), fr$id)],
                     rep("missing_both_parents", 7))
    expect_identical(banner_counts(output$summary_banner)[c("Founders", "Missing parents")],
                     c("Founders" = "0", "Missing parents" = "10"))
    expect_identical(run_settings()$founders_file, "none")
    expect_identical(settings_of(output$download_validate_all)[["Founders file"]], "none")
  })
})

test_that("the downloads give the results workbook, the validated pedigree and the plot", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  fnd  <- upload_example("example_founders.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno, founders_file = fnd)
    session$setInputs(run_validate = 1)
    report <- validate_results()

    xlsx <- output$download_validate_all
    expect_identical(basename(xlsx), paste0("validate_pedigree_results_", Sys.Date(), ".xlsx"))
    expect_identical(openxlsx::getSheetNames(xlsx),
                     c("All Trios", "Pass", "Fail", "Low Markers", "No Genotype Data", "Founders",
                       "Missing Parents", "Marker Summary", "Marker Trio Table", "Settings"))
    trios <- read_sheet(xlsx, "All Trios")
    expect_identical(names(trios), c("id", "orig_male_parent", "orig_female_parent", "trio_mendelian_error_pct",
                                     "trio_markers_tested", "status", "issue", "recommended_correction",
                                     "recommended_action", "male_parent_hom_error_pct",
                                     "female_parent_hom_error_pct"))
    expect_equal(trios, validation_issues(report$full_results, 5, 10), ignore_attr = TRUE)
    issue  <- stats::setNames(trios$issue, trios$id)
    action <- stats::setNames(trios$recommended_action, trios$id)
    expect_match(issue[["C04"]], "^fail: trio error [0-9]+\\.[0-9]{2}% > 5%$")
    expect_match(issue[["C08"]], "^low markers: [0-9]+ testable markers < 10$")
    expect_identical(unname(issue[c("C07", "C10", "M4", "M1", "C01")]),
                     c("male parent unknown", "female parent unknown",
                       "no genotype data for the offspring or a parent", "none", "none"))
    expect_identical(unname(action[c("C04", "C05", "C06", "C11", "C08")]),
                     c("Set male parent to missing", "Set female parent to missing", "Set both parents to missing",
                       "Unresolved: review by hand", "Low markers: keep both parents"))
    expect_identical(nrow(read_sheet(xlsx, "Marker Trio Table")), 300L)
    expect_identical(names(read_sheet(xlsx, "Marker Summary")),
                     c("marker", "trios_tested", "trios_mismatch", "mismatch_pct"))

    s <- settings_of(xlsx)
    expect_identical(names(s), c("Date", "Familia version", "BIGpopA version", "Tab", "Pedigree file",
                                 "Genotypes file", "Founders file", "Trio error threshold (%)",
                                 "Single parent error threshold (%)", "Min markers", "Ploidy"))
    expect_identical(unname(s[-(1:3)]),
                     c("Validate Pedigree", "example_pedigree_cleaned.txt", "example_genotypes.txt",
                       "example_founders.txt", "5", "2", "10", "2"))

    txt <- output$download_pedigree
    expect_identical(basename(txt), "pedigree_validated.txt")
    expect_identical(readLines(txt), table_lines(report$corrected_pedigree))
    expect_identical(readLines(txt), readLines(extdata("example_pedigree_validated.txt")))

    session$setInputs(plot_image_type = "png", plot_image_width = 3, plot_image_height = 2, plot_image_res = 50)
    img <- output$download_validate_plot
    expect_identical(basename(img), paste0("validate_pedigree_plot_", Sys.Date(), ".png"))
    expect_identical(file_magic(img, 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  })
})

test_that("hovering over a bar of the Mendelian Error Plot counts its trios and clicking lists them", {
  skip_on_cran()   # draws the plot as a PNG
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  fnd  <- upload_example("example_founders.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    expect_error(output$error_plot, class = "shiny.silent.error")
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno, founders_file = fnd)
    session$setInputs(run_validate = 1)
    report <- validate_results()

    # the plot is returned, not printed, so clicks arrive in error % (data units)
    expect_identical(output$error_plot$coordmap$panels[[1]]$mapping$x, "trio_mendelian_error_pct")
    expect_identical(output$error_plot_hint, "Click a bar to list its trios under the plot.")
    expect_null(output$error_plot_bar_ui)

    bar <- fullest_bar(report$plot)
    session$setInputs(error_plot_hover = list(x = bar$x, y = 1))
    expect_identical(output$error_plot_hint,
                     paste0("Bar ", bar$label, ": ", bar$n, " trio(s). Click to list them."))

    session$setInputs(error_plot_click = list(x = bar$x, y = 1))
    rows <- selected_bar()$rows
    expect_identical(selected_bar()$label, bar$label)
    expect_identical(nrow(rows), bar$n)                       # the trios counted in the bar
    expect_true(all(rows$trio_mendelian_error_pct >= bar$lo & rows$trio_mendelian_error_pct <= bar$hi))
    expect_identical(names(rows), names(plain(report$full_results)))
    bar_ui <- ui_html(output$error_plot_bar_ui)
    expect_match(bar_ui, paste0("Trios with a trio error of ", bar$label, " (", bar$n, ")"), fixed = TRUE)
    expect_match(bar_ui, paste0('id="', session$ns("error_plot_bar_table"), '"'), fixed = TRUE)

    # outside the bars: no count, nothing listed
    session$setInputs(error_plot_hover = list(x = 99, y = 1))
    expect_identical(output$error_plot_hint, "Click a bar to list its trios under the plot.")
    session$setInputs(error_plot_click = list(x = 99, y = 1))
    expect_null(selected_bar())
    expect_null(output$error_plot_bar_ui)

    # a new run clears the list
    session$setInputs(error_plot_click = list(x = bar$x, y = 1))
    expect_false(is.null(selected_bar()))
    session$setInputs(run_validate = 2)
    expect_null(selected_bar())
  })
})

test_that("VCF and .ped genotypes give the same validation, and a wrong Ploidy stops the run", {
  ped    <- upload_example("example_pedigree_cleaned.txt")
  txt    <- upload_example("example_genotypes.txt")
  vcf    <- upload_example("example_genotypes.vcf")
  plink  <- upload_example("example_genotypes.ped")
  fnd    <- upload_example("example_founders.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log <- record_session(session)
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = txt, founders_file = fnd)
    session$setInputs(run_validate = 1)
    from_txt <- plain(validate_results()$full_results)

    session$setInputs(genotypes_file = vcf)
    session$setInputs(run_validate = 2)
    expect_identical(plain(validate_results()$full_results), from_txt)
    expect_identical(run_settings()$genotypes_file, "example_genotypes.vcf")

    session$setInputs(genotypes_file = plink)
    session$setInputs(run_validate = 3)
    expect_identical(plain(validate_results()$full_results), from_txt)

    session$setInputs(ploidy = 4)
    session$setInputs(run_validate = 4)
    expect_null(validate_results())
    expect_identical(last_progress(log)[c("status", "title")],
                     list(status = "danger", title = "Failed: PLINK .ped files are diploid; set Ploidy to 2."))
    expect_identical(button_state(log, "download_validate_all"), "disabled")

    session$setInputs(genotypes_file = vcf)
    session$setInputs(run_validate = 5)
    expect_null(validate_results())
    expect_identical(last_progress(log)$title,
                     paste("Failed: The VCF looks diploid (ploidy 2): most genotype calls have 2 alleles,",
                           "but Ploidy is set to 4. Set Ploidy to 2 and run again."))
  })
})

test_that("a threshold out of range stops the run with BIGpopA's message", {
  ped  <- upload_example("example_pedigree_cleaned.txt")
  geno <- upload_example("example_genotypes.txt")
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log <- record_session(session)
    session$setInputs(!!!validate_defaults, pedigree_file = ped, genotypes_file = geno)
    session$setInputs(trio_error_threshold = 150, run_validate = 1)
    expect_null(validate_results())
    expect_identical(last_progress(log)$title, "Failed: trio_error_threshold must be between 0 and 100")
  })
})

test_that("Help opens the Validate Pedigree help and the example link gives a zip of the example files", {
  shiny::testServer(mod_validate_ped_server, args = validate_args, {
    log <- record_session(session)
    session$setInputs(help_btn = 1)
    expect_length(log$modals, 1)
    expect_match(log$modals, "Validate Pedigree - Help", fixed = TRUE)
    expect_match(log$modals, 'id="modal_vp_help_fail"', fixed = TRUE)

    zip_path <- output$download_examples
    expect_identical(basename(zip_path), "validate_pedigree_examples.zip")
    expect_identical(zip::zip_list(zip_path)$filename, example_files$validate_ped)
  })
})
