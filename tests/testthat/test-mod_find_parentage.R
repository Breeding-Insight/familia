testthat::local_edition(3)

# Find Parentage tab (R/mod_find_parentage.R), run with shiny::testServer() on
# the example files. The true parents of C01-C38 are example_truth
# (helper-familia.R); C08 has 8 markers called, C09's male parent M4 is not
# genotyped, C11 is a contaminated sample, and M1c has the same calls as M1.

parentage_args <- list(parent_session = NULL)

# BIGpopA::find_parentage() called the way the tab calls it
parentage_direct <- function(geno, parents, progeny, method = "best_pair", founders = NULL, top_n = 1L) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  BIGpopA::find_parentage(
    genotypes_file       = read_genotype_upload(geno, ploidy = NULL),
    parents_file         = read_table_upload(parents),
    progeny_file         = read_table_upload(progeny),
    method               = method,
    min_markers          = 10L,
    error_threshold      = 5,
    show_ties            = TRUE,
    allow_parent_selfing = FALSE,
    exclude_self_match   = TRUE,
    ploidy               = 2L,
    top_n                = top_n,
    founders_file        = if (is.null(founders)) NULL else founders$datapath,
    verbose              = FALSE,
    plot_results         = TRUE
  )
}
tile_labels_fp <- c("Candidate parents", "Male (M)", "Female (F)", "Either sex (A)", "Progeny",
                    "Genotyped individuals", "Markers")

# <option> tag of Fill in pedigree in an update of the Method picker
fill_option <- function(msg) {
  html <- paste(unlist(msg), collapse = " ")
  regmatches(html, regexpr('<option[^>]*value="fill_pedigree"[^>]*>', html, perl = TRUE))
}

test_that("the tiles count the candidate parents by sex, the progeny and the genotypes", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    expect_identical(tiles(output$dashboard), stats::setNames(rep(DASH, 7), tile_labels_fp))
    session$setInputs(parents_file = parents)
    expect_identical(tiles(output$dashboard),
                     stats::setNames(c("7", "4", "3", "0", DASH, DASH, DASH), tile_labels_fp))
    session$setInputs(progeny_file = progeny, genotypes_file = geno)
    expect_identical(tiles(output$dashboard),
                     stats::setNames(c("7", "4", "3", "0", "38", "45", "300"), tile_labels_fp))
    expect_identical(tiles_note(output$dashboard), NA_character_)
  })
})

test_that("uploading the genotypes, parents or progeny opens the Data Overview tab", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  fnd     <- upload_example("example_founders.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log  <- record_session(session)
    tabs <- function() input_updates(log, "parentage_results_tabs")
    session$flushReact()                                       # start-up, no file
    expect_length(tabs(), 0)
    session$setInputs(genotypes_file = geno)
    session$setInputs(parents_file = parents)
    session$setInputs(progeny_file = progeny)
    expect_identical(tabs(), rep(list(list(value = "Data Overview")), 3))
    session$setInputs(founders_file = fnd, method = "best_match")   # not counted in the overview
    expect_length(tabs(), 3)
  })
})

test_that("sex is read in any case, and parents without a sex column count as either sex", {
  odd   <- upload_text(c("ID\tSex", "P1\tm", "P2\t F", "P3\tA", "P4\tx"), "parents.txt")
  nosex <- upload_text(c("id", "P1", "P2"), "parents.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    session$setInputs(parents_file = odd)
    expect_identical(tiles(output$dashboard)[1:4],
                     c("Candidate parents" = "4", "Male (M)" = "1", "Female (F)" = "1", "Either sex (A)" = "2"))
    session$setInputs(parents_file = nosex)
    expect_identical(tiles(output$dashboard)[1:4],
                     c("Candidate parents" = "2", "Male (M)" = "0", "Female (F)" = "0", "Either sex (A)" = "2"))
  })
})

test_that("files that cannot be read are named in the note under the tiles", {
  empty <- upload_text(character(0), "empty.txt")
  gz    <- upload_text("x", "genotypes.txt.gz")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    session$setInputs(parents_file = empty, progeny_file = empty, genotypes_file = gz)
    expect_identical(tiles(output$dashboard), stats::setNames(rep(DASH, 7), tile_labels_fp))
    expect_identical(tiles_note(output$dashboard),
                     paste("Could not read the parents file: no lines available in input",
                           "Could not read the progeny file: no lines available in input",
                           "Could not read the genotypes file: Compressed genotype files must be VCFs (.vcf.gz)."))
  })
})

test_that("Fill in pedigree is offered only when the progeny file is a pedigree", {
  progeny  <- upload_example("example_progeny.txt")
  pedigree <- upload_example("example_pedigree_validated.txt")
  odd_case <- upload_text(c("ID\tMale Parent\tfemale.parent", "A\t0\t0"), "pedigree.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(method = "best_pair", progeny_file = progeny)
    msg <- input_updates(log, "method")[[1]]
    expect_match(fill_option(msg), "disabled", fixed = TRUE)
    expect_identical(msg$value, "best_pair")

    session$setInputs(progeny_file = pedigree)
    msg <- input_updates(log, "method")[[2]]
    expect_false(grepl("disabled", fill_option(msg), fixed = TRUE))
    expect_identical(msg$value, "best_pair")

    session$setInputs(progeny_file = odd_case)   # column names in any case and spacing
    expect_false(grepl("disabled", fill_option(input_updates(log, "method")[[3]]), fixed = TRUE))

    # a progeny list after Fill in pedigree was chosen goes back to Best pair
    session$setInputs(method = "fill_pedigree", progeny_file = progeny)
    msg <- input_updates(log, "method")[[4]]
    expect_match(fill_option(msg), "disabled", fixed = TRUE)
    expect_identical(msg$value, "best_pair")
  })
})

test_that("Run Parentage Assignment needs the genotypes, parents and progeny files", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, run_parentage = 1)
    expect_null(parentage_results())
    expect_length(progress_updates(log), 0)
  })
})

test_that("Best pair on the example files finds the true parents of every planted progeny", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  direct  <- parentage_direct(geno, parents, progeny)
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = progeny)
    session$setInputs(run_parentage = 1)
    report <- parentage_results()

    for (nm in c("pass", "high_error", "low_markers", "full_results")) {
      expect_identical(plain(report[[nm]]), plain(direct[[nm]]), label = nm)
    }
    expect_true(inherits(report$plot, "ggplot"))

    fr  <- plain(report$full_results)
    expect_setequal(fr$id, example_truth$id)
    got   <- fr[match(example_truth$id, fr$id), ]
    ok    <- !example_truth$id %in% c("C08", "C09", "C11")
    truth <- example_truth[ok, ]
    expect_true(all(got$status[ok] == "pass"))
    expect_identical(got$female_parent[ok], truth$female)
    expect_true(all(got$male_parent[ok] == truth$male | (truth$male == "M1" & got$male_parent[ok] == "M1c")))
    expect_identical(fr$status[match(c("C08", "C09", "C11"), fr$id)], c("low_markers", "high_error", "high_error"))
    expect_identical(fr$tied_candidates[fr$id == "C01"], 2L)   # M1 and M1c have the same calls

    expect_identical(run_settings(), list(
      genotypes_file = "example_genotypes.txt", parents_file = "example_parents.txt",
      progeny_file = "example_progeny.txt", founders_file = "none", method = "best_pair",
      error_threshold = 5, min_markers = 10L, ploidy = 2L, top_n = 1L, show_ties = TRUE,
      allow_parent_selfing = FALSE, exclude_self_match = TRUE
    ))
    expect_identical(progress_titles(log), c("Reading input files...", "Running parentage assignment...",
                                             "Compiling results...", "Finished"))
    expect_identical(last_progress(log)$status, "success")
    expect_identical(button_state(log, "download_parentage_all"), "enabled")
    expect_identical(input_updates(log, "parentage_results_tabs"),
                     list(list(value = "Data Overview"), list(value = "Issue Tables")))   # uploads, then run
    expect_length(log$notes, 0)
  })
})

test_that("the Run Summary and the Issue Tables count each result category", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = progeny)
    session$setInputs(run_parentage = 1)
    expect_identical(banner_headline(output$summary_banner), "3 progeny flagged. Review the Issue Tables tab.")
    expect_identical(banner_counts(output$summary_banner),
                     c("Total progeny" = "38", "Pass" = "35", "High error" = "2", "Low markers" = "1"))
    ui <- output$results_ui
    expect_identical(section_counts(ui), c("Pass" = "35 record(s) found", "High Error" = "2 record(s) found",
                                           "Low Markers" = "1 record(s) found"))
    for (tbl in c("tbl_pass", "tbl_high_error", "tbl_low_markers")) {
      expect_match(ui_html(ui), paste0('id="', session$ns(tbl), '"'), fixed = TRUE)
    }
  })
})

test_that("the results workbook has every progeny with its issue and the run settings", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = progeny)
    session$setInputs(run_parentage = 1)
    report <- parentage_results()

    xlsx <- output$download_parentage_all
    expect_identical(basename(xlsx), paste0("find_parentage_results_", Sys.Date(), ".xlsx"))
    expect_identical(openxlsx::getSheetNames(xlsx), c("All Progeny", "Pass", "High Error", "Low Markers", "Settings"))
    all_p <- read_sheet(xlsx, "All Progeny")
    expect_identical(names(all_p)[1:8], c("id", "male_parent", "female_parent", "mendelian_error_pct",
                                          "markers_tested", "status", "issue", "tied_candidates"))
    expect_equal(all_p, parentage_issues(report$full_results, 5, 10), ignore_attr = TRUE)
    issue <- stats::setNames(all_p$issue, all_p$id)
    expect_match(issue[["C09"]], "^high error: best error [0-9]+\\.[0-9]{2}% > 5%$")
    expect_match(issue[["C11"]], "^high error: best error [0-9]+\\.[0-9]{2}% > 5%$")
    expect_match(issue[["C08"]], "^low markers: [0-9]+ markers tested < 10$")
    expect_identical(issue[["C01"]], "2 tied candidates")
    expect_identical(nrow(read_sheet(xlsx, "Pass")), 35L)

    s <- settings_of(xlsx)
    expect_identical(names(s), c("Date", "Familia version", "BIGpopA version", "Tab", "Genotypes file",
                                 "Parents file", "Progeny file", "Founders file", "Method", "Error threshold (%)",
                                 "Min markers", "Ploidy", "Top N matches", "Show ties", "Allow parent selfing",
                                 "Exclude self-match"))
    expect_identical(unname(s[-(1:3)]),
                     c("Find Parentage", "example_genotypes.txt", "example_parents.txt", "example_progeny.txt",
                       "none", "best_pair", "5", "10", "2", "1", "TRUE", "FALSE", "TRUE"))

    session$setInputs(plot_image_type = "png", plot_image_width = 3, plot_image_height = 2, plot_image_res = 50)
    img <- output$download_parentage_plot
    expect_identical(basename(img), paste0("find_parentage_plot_", Sys.Date(), ".png"))
    expect_identical(file_magic(img, 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  })
})

test_that("Top N reports the next best candidates in suffix columns", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  direct  <- parentage_direct(geno, parents, progeny, top_n = 2L)
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = progeny)
    session$setInputs(top_n = 2, run_parentage = 1)
    fr <- plain(parentage_results()$full_results)
    expect_identical(fr, plain(direct$full_results))
    expect_true(all(c("male_parent_2", "female_parent_2", "mendelian_error_pct_2", "markers_tested_2") %in% names(fr)))
    expect_identical(run_settings()$top_n, 2L)
    expect_identical(settings_of(output$download_parentage_all)[["Top N matches"]], "2")
  })
})

test_that("Fill in pedigree searches only the missing parents and skips the founders", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  ped     <- upload_example("example_pedigree_validated.txt")
  fnd     <- upload_example("example_founders.txt")
  direct  <- parentage_direct(geno, parents, ped, method = "fill_pedigree", founders = fnd)
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = ped,
                      founders_file = fnd)
    session$setInputs(method = "fill_pedigree", run_parentage = 1)
    fr <- plain(parentage_results()$full_results)
    expect_identical(fr, plain(direct$full_results))
    expect_setequal(fr$id, c("C04", "C05", "C06", "C07", "C09", "C10"))

    filled <- c("C04", "C05", "C06", "C07", "C10")
    got    <- fr[match(filled, fr$id), ]
    truth  <- example_truth[match(filled, example_truth$id), ]
    expect_identical(got$status, rep("pass", 5))
    expect_identical(got$male_parent, truth$male)
    expect_identical(got$female_parent, truth$female)
    expect_identical(got$search_mode[1:3], c("known_female", "known_male", "pair"))   # C04 0 x F2, C05 M2 x 0, C06 0 x 0
    expect_identical(fr$status[fr$id == "C09"], "high_error")
    expect_length(log$notes, 0)
    expect_identical(run_settings()[c("method", "founders_file")],
                     list(method = "fill_pedigree", founders_file = "example_founders.txt"))
  })
})

test_that("Fill in pedigree without a founders file warns that the founders are searched too", {
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  ped     <- upload_example("example_pedigree_validated.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = ped)
    warnings <- collect_warnings(session$setInputs(method = "fill_pedigree", run_parentage = 1))
    expect_identical(warnings, "The following progeny IDs were not in the genotype file and will not be analyzed: M4")
    expect_identical(log$notes, paste("No founders file: every individual with both parents unknown, founders",
                                      "included, gets a full parent search."))
    fr <- plain(parentage_results()$full_results)
    expect_setequal(fr$id, c("C04", "C05", "C06", "C07", "C09", "C10", "M1", "M1c", "M2", "M3", "F1", "F2", "F3"))
    expect_identical(run_settings()$founders_file, "none")
  })
})

test_that("a run that cannot be done names the reason in the Status box", {
  geno     <- upload_example("example_genotypes.txt")
  plink    <- upload_example("example_genotypes.ped")
  parents  <- upload_example("example_parents.txt")
  progeny  <- upload_example("example_progeny.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(!!!parentage_defaults, genotypes_file = plink, parents_file = parents, progeny_file = progeny)
    session$setInputs(ploidy = 4, run_parentage = 1)
    expect_null(parentage_results())
    expect_identical(last_progress(log)[c("status", "title")],
                     list(status = "danger", title = "Failed: PLINK .ped files are diploid; set Ploidy to 2."))
    expect_identical(button_state(log, "download_parentage_all"), "disabled")

    # Fill in pedigree needs a pedigree, not a list of progeny
    session$setInputs(genotypes_file = geno, ploidy = 2, method = "fill_pedigree", run_parentage = 2)
    expect_null(parentage_results())
    expect_identical(last_progress(log)$title,
                     paste("Failed: method = 'fill_pedigree' requires a progeny_file with id, male_parent",
                           "and female_parent columns."))
  })
})

test_that("hovering over a bar of the Mendelian Error Plot counts its progeny and clicking lists them", {
  skip_on_cran()   # draws the plot as a PNG
  geno    <- upload_example("example_genotypes.txt")
  parents <- upload_example("example_parents.txt")
  progeny <- upload_example("example_progeny.txt")
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    expect_error(output$error_plot, class = "shiny.silent.error")
    session$setInputs(!!!parentage_defaults, genotypes_file = geno, parents_file = parents, progeny_file = progeny)
    session$setInputs(run_parentage = 1)
    report <- parentage_results()

    expect_identical(output$error_plot$coordmap$panels[[1]]$mapping$x, "mendelian_error_pct")
    expect_identical(output$error_plot_hint, "Click a bar to list its progeny under the plot.")

    ld    <- ggplot2::layer_data(report$plot)
    bins  <- unique(ld[, c("xmin", "xmax")])
    total <- vapply(seq_len(nrow(bins)), function(i) sum(ld$count[ld$xmin == bins$xmin[i]]), numeric(1))
    i     <- which.max(total)
    x     <- (bins$xmin[i] + bins$xmax[i]) / 2
    n     <- as.integer(total[i])
    label <- sprintf("%s to %s%%", format(max(0, bins$xmin[i])), format(bins$xmax[i]))

    session$setInputs(error_plot_hover = list(x = x, y = 1))
    expect_identical(output$error_plot_hint, paste0("Bar ", label, ": ", n, " progeny. Click to list them."))
    session$setInputs(error_plot_click = list(x = x, y = 1))
    rows <- selected_bar()$rows
    expect_identical(nrow(rows), n)
    err <- as.numeric(rows$mendelian_error_pct)
    expect_true(all(err >= bins$xmin[i] & err <= bins$xmax[i]))
    expect_match(ui_html(output$error_plot_bar_ui), paste0("Progeny with an error of ", label, " (", n, ")"),
                 fixed = TRUE)

    session$setInputs(error_plot_click = list(x = 99, y = 1))
    expect_null(selected_bar())
    expect_null(output$error_plot_bar_ui)
  })
})

test_that("Help opens the Find Parentage help and the example link gives a zip of the example files", {
  shiny::testServer(mod_find_parentage_server, args = parentage_args, {
    log <- record_session(session)
    session$setInputs(help_btn = 1)
    expect_length(log$modals, 1)
    expect_match(log$modals, "Find Parentage - Help", fixed = TRUE)
    expect_match(log$modals, 'id="modal_fp_help_fill_pedigree"', fixed = TRUE)

    zip_path <- output$download_examples
    expect_identical(basename(zip_path), "find_parentage_examples.zip")
    expect_identical(zip::zip_list(zip_path)$filename, example_files$find_parentage)
  })
})
