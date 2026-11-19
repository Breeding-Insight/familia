testthat::local_edition(3)

# SNMF tab (R/mod_SNMF.R), run with shiny::testServer(). The tiles need no LEA;
# the runs need LEA (Bioconductor) and are skipped without it, and on CRAN
# because they take a few seconds. The example VCF (73 tetraploid samples of
# three lines) gives its lowest cross-entropy at K = 3 with the settings of the
# guide (data-raw/make_example_data.R).

tetra_msg   <- paste("The VCF looks tetraploid (ploidy 4): most genotype calls have 4 alleles,",
                     "but Ploidy is set to 2. Set Ploidy to 4 and run again.")
snmf_guide  <- utils::modifyList(snmf_defaults, list(snmf_ploidy = 4, snmf_k_max = 5, snmf_repetitions = 3))
snmf_quick  <- utils::modifyList(snmf_defaults, list(snmf_ploidy = 4, snmf_k_min = 2, snmf_k_max = 3,
                                                     snmf_repetitions = 1, snmf_iterations = 20,
                                                     snmf_select_mode = "no_entropy"))
example_dose <- function() BIGpopA::vcf_to_dosage(extdata("example_snmf.vcf"), format = "matrix", verbose = FALSE)

test_that("the tiles count the samples, markers, ploidy and missing calls of a VCF or a .geno", {
  vcf       <- upload_example("example_snmf.vcf")
  geno_path <- withr::local_tempfile(fileext = ".geno")
  write_geno_upload(vcf, geno_path, ploidy = 4)
  geno      <- as_upload(geno_path, "example_snmf.geno")
  missing   <- format_pct(mean(is.na(example_dose())))
  shiny::testServer(mod_SNMF_server, {
    expect_identical(tiles(output$dashboard),
                     c("Samples" = DASH, "Markers" = DASH, "Detected ploidy" = DASH, "Missing calls" = DASH))
    session$setInputs(snmf_file = vcf)
    expect_identical(tiles(output$dashboard),
                     c("Samples" = "73", "Markers" = "300", "Detected ploidy" = "4", "Missing calls" = missing))
    session$setInputs(snmf_file = geno)   # a .geno has no ploidy to detect
    expect_identical(tiles(output$dashboard),
                     c("Samples" = "73", "Markers" = "300", "Detected ploidy" = DASH, "Missing calls" = missing))
  })
})

test_that("uploading the genotypes opens the Data Overview tab", {
  vcf <- upload_example("example_snmf.vcf")
  shiny::testServer(mod_SNMF_server, {
    log  <- record_session(session)
    tabs <- function() input_updates(log, "snmf_results_tabs")
    session$flushReact()                                   # start-up, no file
    expect_length(tabs(), 0)
    session$setInputs(snmf_file = vcf)
    expect_identical(tabs(), list(list(value = "Data Overview")))
    session$setInputs(snmf_ploidy = 4, snmf_k_max = 5)     # other inputs do not open it
    expect_length(tabs(), 1)
  })
})

test_that("a genotypes file that cannot be read is named under the tiles", {
  bad <- upload_text(c("id\tm1\tm2", "A\t0"), "genotypes.txt")
  shiny::testServer(mod_SNMF_server, {
    session$setInputs(snmf_file = bad)
    expect_true(all(tiles(output$dashboard) == DASH))
    expect_match(tiles_note(output$dashboard), "^Could not read the genotypes file: .*did not have 3 elements")
  })
})

test_that("before a run the K selector, value boxes and plots wait for SNMF", {
  shiny::testServer(mod_SNMF_server, {
    expect_identical(ui_html(output$snmf_selectors_ui), "<em>Run SNMF to enable the K selector and downloads.</em>")
    expect_match(ui_html(output$snmf_best_k_box), "Best K", fixed = TRUE)
    expect_match(ui_html(output$snmf_best_k_box), DASH, fixed = TRUE)
    expect_match(ui_html(output$snmf_best_ce_box), DASH, fixed = TRUE)
    expect_error(output$snmf_ce_plot, "Cross-entropy disabled (see Selection mode).", fixed = TRUE)
  })
})

test_that("without LEA, Run SNMF explains how to install it", {
  skip_if(requireNamespace("LEA", quietly = TRUE), "LEA is installed")
  shiny::testServer(mod_SNMF_server, {
    log <- record_session(session)
    session$setInputs(!!!snmf_defaults, snmf_run = 1)
    expect_length(log$modals, 1)
    expect_match(log$modals, "LEA is not installed", fixed = TRUE)
    expect_match(log$modals, 'BiocManager::install("LEA")', fixed = TRUE)
    expect_null(state$project)
  })
})

test_that("Run SNMF checks the file, K range, repetitions and ploidy before running", {
  skip_if_not_installed("LEA")
  vcf <- upload_example("example_snmf.vcf")
  txt <- upload_text(c("id\tm1", "A\t0"), "genotypes.txt")
  shiny::testServer(mod_SNMF_server, {
    log <- record_session(session)
    session$setInputs(!!!snmf_defaults, snmf_run = 1)
    expect_match(log$modals[1], "Missing input", fixed = TRUE)
    expect_match(log$modals[1], "Upload a .vcf/.vcf.gz or .geno file.", fixed = TRUE)

    session$setInputs(snmf_file = vcf, snmf_k_min = 3, snmf_k_max = 2, snmf_run = 2)
    expect_match(log$modals[2], "Invalid K range", fixed = TRUE)
    session$setInputs(snmf_k_min = 1, snmf_repetitions = 0, snmf_run = 3)
    expect_match(log$modals[3], "Invalid repetitions", fixed = TRUE)
    session$setInputs(snmf_repetitions = 1, snmf_ploidy = 0, snmf_run = 4)
    expect_match(log$modals[4], "Invalid ploidy", fixed = TRUE)
    expect_null(state$k_values)   # nothing was started
    expect_null(state$project)

    session$setInputs(snmf_ploidy = 2, snmf_run = 5)   # the example is tetraploid
    expect_match(log$modals[5], "VCF conversion failed", fixed = TRUE)
    expect_match(log$modals[5], tetra_msg, fixed = TRUE)
    expect_identical(output$snmf_status, paste0("ERROR: ", tetra_msg))
    expect_null(state$project)
    expect_identical(button_state(log, "download_snmf_all"), "disabled")

    session$setInputs(snmf_file = txt, snmf_ploidy = 4, snmf_run = 6)
    expect_match(log$modals[6], "Unsupported file type", fixed = TRUE)
    expect_identical(output$snmf_status, "ERROR: Unsupported file type.")
    expect_length(log$modals, 6)
  })
})

test_that("SNMF on the example VCF finds K = 3 and uses the best run of each K", {
  skip_on_cran()
  skip_if_not_installed("LEA")
  vcf     <- upload_example("example_snmf.vcf")
  ids     <- rownames(example_dose())
  out_dir <- withr::local_tempdir()
  seen    <- new.env()
  shiny::testServer(mod_SNMF_server, {
    log <- record_session(session)
    session$setInputs(!!!snmf_guide, snmf_file = vcf)
    quietly(session$setInputs(snmf_run = 1))
    seen$run_dir <- state$run_dir
    expect_true(dir.exists(state$run_dir))

    expect_false(is.null(state$project))
    expect_identical(state$k_values, 1:5)
    expect_identical(state$repetitions, 3L)
    expect_true(state$entropy_enabled)
    expect_identical(state$sample_ids, ids)
    expect_identical(basename(state$geno_path), "example_snmf.geno")

    ce <- state$ce_df
    expect_identical(names(ce), c("K", "run", "cross_entropy"))
    expect_identical(ce$K, rep(1:5, each = 3))
    expect_identical(ce$run, rep(1:3, times = 5))
    expect_false(anyNA(ce$cross_entropy))
    best_run <- vapply(1:5, function(k) ce$run[ce$K == k][which.min(ce$cross_entropy[ce$K == k])], integer(1))
    expect_identical(state$ce_summary$K, 1:5)
    expect_identical(state$ce_summary$best_run, best_run)
    expect_identical(state$ce_summary$min_cross_entropy,
                     vapply(1:5, function(k) min(ce$cross_entropy[ce$K == k]), numeric(1)))
    expect_identical(state$best_k, 3L)

    expect_identical(selected_k(), 3L)
    expect_identical(selected_run(), best_run[3])
    q <- q_matrix()
    expect_identical(dim(q), c(73L, 3L))
    expect_identical(rownames(q), ids)
    expect_identical(colnames(q), paste0("Cluster", 1:3))
    expect_equal(unname(rowSums(q)), rep(1, 73), tolerance = 1e-4)

    expect_identical(output$snmf_status, "SNMF complete.")
    expect_identical(button_state(log, "download_snmf_all"), "enabled")
    expect_identical(input_updates(log, "snmf_selected_k")[[1]]$value, "3")
    expect_match(ui_html(output$snmf_best_k_box), ">\\s*3\\s*<")
    expect_match(ui_html(output$snmf_best_ce_box),
                 as.character(round(min(state$ce_summary$min_cross_entropy), 6)), fixed = TRUE)
    expect_match(ui_html(output$snmf_selectors_ui), paste0('id="', session$ns("snmf_selected_k"), '"'), fixed = TRUE)

    # Advanced Options: another run of the same K, then back to the best one
    session$setInputs(snmf_advanced_run = 1)
    expect_match(log$modals[length(log$modals)], paste0("currently run ", best_run[3], " for K = 3"), fixed = TRUE)
    session$setInputs(snmf_run_override = "2")
    expect_identical(state$run_override, 2L)
    expect_identical(selected_run(), 2L)
    session$setInputs(snmf_reset_run_override = 1)
    expect_identical(state$run_override, NA_integer_)
    expect_identical(input_updates(log, "snmf_run_override")[[1]]$value, "auto")
    expect_identical(selected_run(), best_run[3])

    # choosing another K drops the override
    session$setInputs(snmf_run_override = "1")
    session$setInputs(snmf_selected_k = "2")
    expect_identical(state$run_override, NA_integer_)
    expect_identical(selected_k(), 2L)
    expect_identical(selected_run(), best_run[2])
    expect_identical(dim(q_matrix()), c(73L, 2L))

    # downloads: Q of the selected K and run, and every cross-entropy
    zip_path <- output$download_snmf_all
    q_file   <- paste0("snmf_Q_K2_run", best_run[2], ".csv")
    expect_identical(basename(zip_path), paste0("snmf_results_", Sys.Date(), ".zip"))
    expect_setequal(zip::zip_list(zip_path)$filename, c(q_file, "snmf_cross_entropy.csv"))
    zip::unzip(zip_path, exdir = out_dir)
    q_csv <- utils::read.csv(file.path(out_dir, q_file), check.names = FALSE)
    expect_identical(names(q_csv), c("ID", "Cluster1", "Cluster2"))
    expect_identical(q_csv$ID, ids)
    expect_equal(unname(as.matrix(q_csv[, -1])), unname(q_matrix()), tolerance = 1e-6)
    expect_equal(utils::read.csv(file.path(out_dir, "snmf_cross_entropy.csv")), state$ce_df, ignore_attr = TRUE)

    session$setInputs(snmf_figure = "Cross-Entropy Plot", snmf_image_type = "png", snmf_image_width = 3,
                      snmf_image_height = 2, snmf_image_res = 50)
    img <- output$download_snmf_figure
    expect_identical(basename(img), paste0("snmf_cross_entropy_", Sys.Date(), ".png"))
    expect_identical(file_magic(img, 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
    session$setInputs(snmf_figure = "Ancestry Plot")
    expect_identical(basename(output$download_snmf_figure), paste0("snmf_ancestry_plot_", Sys.Date(), ".png"))
  })
  expect_false(dir.exists(seen$run_dir))   # the run folder is deleted when the session ends
})

test_that("without cross-entropy the first K is used and the cross-entropy outputs say why they are empty", {
  skip_on_cran()
  skip_if_not_installed("LEA")
  vcf <- upload_example("example_snmf.vcf")
  shiny::testServer(mod_SNMF_server, {
    session$setInputs(!!!snmf_quick, snmf_file = vcf)
    quietly(session$setInputs(snmf_run = 1))
    expect_false(state$entropy_enabled)
    expect_identical(state$best_k, 2L)
    expect_null(state$ce_df)
    expect_null(state$ce_summary)
    expect_identical(selected_run(), 1L)
    expect_identical(dim(q_matrix()), c(73L, 2L))
    expect_error(output$snmf_ce_plot, "Cross-entropy disabled (see Selection mode).", fixed = TRUE)
    expect_match(ui_html(output$snmf_best_ce_box), DASH, fixed = TRUE)
    expect_identical(zip::zip_list(output$download_snmf_all)$filename, "snmf_Q_K2_run1.csv")
  })
})

test_that("a .geno upload is run as it is", {
  skip_on_cran()
  skip_if_not_installed("LEA")
  vcf       <- upload_example("example_snmf.vcf")
  geno_path <- withr::local_tempfile(fileext = ".geno")
  write_geno_upload(vcf, geno_path, ploidy = 4)
  geno      <- as_upload(geno_path, "example_snmf.geno")
  one_k     <- utils::modifyList(snmf_quick, list(snmf_k_max = 2))
  shiny::testServer(mod_SNMF_server, {
    session$setInputs(!!!one_k, snmf_file = geno)
    quietly(session$setInputs(snmf_run = 1))
    expect_false(is.null(state$project))
    expect_identical(basename(state$geno_path), "example_snmf.geno")
    expect_identical(readLines(state$geno_path), readLines(geno_path))   # copied unchanged
    expect_null(state$sample_ids)
    expect_identical(nrow(q_matrix()), 73L)
    expect_identical(output$snmf_status, "SNMF complete.")
  })
})

test_that("Help opens the SNMF help and the example link gives the tetraploid example VCF", {
  shiny::testServer(mod_SNMF_server, {
    log <- record_session(session)
    session$setInputs(help_btn = 1)
    expect_match(log$modals, "SNMF - Help", fixed = TRUE)
    expect_match(log$modals, 'id="modal_snmf_help_auto_entropy"', fixed = TRUE)
    path <- output$download_examples
    expect_identical(basename(path), "example_snmf.vcf")
    expect_identical(readLines(path), readLines(extdata("example_snmf.vcf")))
  })
})
