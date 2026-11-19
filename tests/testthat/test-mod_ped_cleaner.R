testthat::local_edition(3)

# Pedigree Cleaner tab (R/mod_ped_cleaner.R), run with shiny::testServer().
# The example pedigree has one case of each issue (data-raw/make_example_data.R):
# C02 is duplicated, C07 has two male parents (M2 and M1), M4 has no row and is
# the male parent of C09 and the female parent of C10, and X1 <-> X2 is a cycle.

cleaner_args <- list(parent_session = NULL)

example_counts <- c("Exact duplicates" = "2", "Conflicting trios" = "1 id(s) (0 kept by you, 1 set to missing)",
                    "Inconsistent sex roles" = "2", "Missing parents" = "1", "Cycles / dependencies" = "1")

test_that("the tiles show a dash before upload and count the uploaded pedigree", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    expect_identical(tiles(output$dashboard),
                     stats::setNames(rep(DASH, 5), c("Records", "Unique IDs", "Complete trios",
                                                     "One parent known", "Both parents unknown")))
    expect_identical(tiles_note(output$dashboard), NA_character_)

    session$setInputs(ped_file = ped)
    expect_identical(tiles(output$dashboard),
                     c("Records" = "49", "Unique IDs" = "47", "Complete trios" = "42",
                       "One parent known" = "0", "Both parents unknown" = "7"))
    expect_identical(tiles_note(output$dashboard), NA_character_)
  })
})

test_that("uploading the pedigree opens the Data Overview tab, and nothing else does", {
  ped  <- upload_example("example_pedigree.txt")
  ped2 <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log  <- record_session(session)
    tabs <- function() input_updates(log, "ped_results_tabs")
    session$flushReact()                       # start-up, no file
    expect_length(tabs(), 0)
    session$setInputs(!!!cleaner_defaults)     # the options do not open it
    expect_length(tabs(), 0)
    session$setInputs(ped_file = ped)
    expect_identical(tabs(), list(list(value = "Data Overview")))
    session$setInputs(ped_file = ped2)         # each new upload opens it again
    expect_identical(tabs(), rep(list(list(value = "Data Overview")), 2))
  })
})

test_that("an unreadable pedigree or one without the required columns gives a note and a failed run", {
  empty <- upload_text(character(0), "empty.txt")
  wrong <- upload_text(c("id\tsire\tdam", "A\t0\t0"), "wrong.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(!!!cleaner_defaults, ped_file = empty)
    expect_identical(tiles_note(output$dashboard), "Could not read the pedigree file: no lines available in input")
    expect_true(all(tiles(output$dashboard) == DASH))
    session$setInputs(run_check = 1)
    expect_null(check_results())
    expect_identical(last_progress(log)[c("value", "status", "title")],
                     list(value = 100, status = "danger", title = "Failed: no lines available in input"))

    session$setInputs(ped_file = wrong)
    expect_identical(tiles_note(output$dashboard),
                     "The pedigree file needs id, male_parent and female_parent columns.")
    session$setInputs(run_check = 2)
    expect_null(check_results())
    expect_identical(last_progress(log)$title,
                     paste("Failed: Missing required column(s): male_parent, female_parent.",
                           "File must contain: id, male_parent, female_parent."))
    expect_identical(button_state(log, "download_results"),  "disabled")
    expect_identical(button_state(log, "download_pedigree"), "disabled")
    expect_error(output$summary_banner, class = "shiny.silent.error")
    expect_error(output$results_ui,     class = "shiny.silent.error")
  })
})

test_that("Run Pedigree Check does nothing before a pedigree is uploaded", {
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(!!!cleaner_defaults, run_check = 1)
    expect_null(check_results())
    expect_length(progress_updates(log), 0)
  })
})

test_that("a run on the example pedigree finds one case of each issue", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)

    res      <- check_results()
    uploaded <- read_table_upload(ped)
    expect_identical(res$file_name, "example_pedigree.txt")
    expect_true(res$set_conflicts)
    expect_true(res$set_sex)
    expect_identical(res$ped_input, uploaded)
    expect_identical(res$report, BIGpopA::check_ped(uploaded, verbose = FALSE))
    expect_identical(res$base_report, res$report)
    expect_null(kept_rows())

    r <- res$report
    expect_identical(plain(r$exact_duplicates),
                     data.frame(row_number = c(9L, 10L), id = "C02", male_parent = "M2", female_parent = "F2"))
    expect_identical(plain(r$conflicting_trios),
                     data.frame(row_number = c(14L, 15L), id = "C07", male_parent = c("M2", "M1"),
                                female_parent = "F3"))
    expect_identical(plain(r$inconsistent_sex_roles),
                     data.frame(row_number = c(17L, 18L), id = c("C09", "C10"), male_parent = c("M4", "M3"),
                                female_parent = c("F1", "M4")))
    expect_identical(plain(r$missing_parents),
                     data.frame(row_number = 17L, id = "M4", male_parent = "0", female_parent = "0"))
    expect_identical(plain(r$dependencies),
                     data.frame(dependency = "Cycle detected involving IDs: X1 -> X2 -> X1"))
    expect_identical(plain(r$corrected_pedigree),
                     utils::read.delim(extdata("example_pedigree_cleaned.txt"), colClasses = "character"))

    expect_identical(progress_titles(log),
                     c("Reading pedigree file...",
                       "Checking duplicates, conflicting trios, sex roles, missing parents and cycles...",
                       "Finished"))
    expect_identical(last_progress(log)$status, "success")
    expect_identical(button_state(log, "download_results"),  "enabled")
    expect_identical(button_state(log, "download_pedigree"), "enabled")
    expect_identical(input_updates(log, "ped_results_tabs"),
                     list(list(value = "Data Overview"), list(value = "Issue Tables")))   # upload, then run
  })
})

test_that("the Run Summary and the Issue Tables count each issue", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)

    expect_identical(banner_headline(output$summary_banner), "7 issue(s) detected. Review the Issue Tables tab.")
    expect_identical(banner_counts(output$summary_banner), example_counts)
    expect_match(ui_html(output$summary_banner), "background-color:#fff3cd", fixed = TRUE)

    ui <- output$results_ui
    expect_identical(section_counts(ui),
                     c("Exact Duplicates" = "2 record(s) found", "Conflicting Trios" = "2 record(s) found",
                       "Inconsistent Sex Roles" = "2 record(s) found", "Missing Parents" = "1 record(s) found",
                       "Cycles / Dependencies" = "1 record(s) found"))
    html <- ui_html(ui)
    for (tbl in c("tbl_exact_duplicates", "tbl_conflicting_trios", "tbl_inconsistent_sex_roles",
                  "tbl_missing_parents", "tbl_cycles___dependencies")) {
      expect_match(html, paste0('id="', session$ns(tbl), '"'), fixed = TRUE)
    }
    expect_match(html, paste0('id="', session$ns("apply_keep"), '"'), fixed = TRUE)
    expect_match(html, paste0('id="', session$ns("reset_keep"), '"'), fixed = TRUE)
    expect_match(html, "Ids without a checked row are\\s+set to missing \\(0\\)\\.")
  })
})

test_that("the Run Summary states what each option did", {
  ped <- upload_example("example_pedigree.txt")
  always <- "Exact duplicates are always removed and missing parents always added."
  cases <- list(
    list(TRUE, TRUE, "set to missing)",
         paste(always, "Conflicting parents and parents with inconsistent sex roles were also set to",
               "missing (0) in this run (check help for details).")),
    list(TRUE, FALSE, "set to missing)",
         paste(always, "Conflicting parents were also set to missing (0) in this run; inconsistent sex",
               "roles are only flagged (check help for details).")),
    list(FALSE, TRUE, "flagged)",
         paste(always, "Parents with inconsistent sex roles were also set to missing (0) in this run;",
               "conflicting trios are only flagged (check help for details).")),
    list(FALSE, FALSE, "flagged)",
         paste("Only exact duplicates were removed and missing parents added in this run; the other",
               "issues are only flagged (check help for details)."))
  )
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    uploaded <- read_table_upload(ped)
    for (i in seq_along(cases)) {
      cs <- cases[[i]]
      session$setInputs(ped_file = ped, correct_conflicting_trios = cs[[1]],
                        correct_inconsistent_sex_roles = cs[[2]])
      session$setInputs(run_check = i)
      expect_identical(check_results()$report,
                       BIGpopA::check_ped(uploaded, verbose = FALSE, correct_conflicting_trios = cs[[1]],
                                          correct_inconsistent_sex_roles = cs[[2]]))
      expect_identical(banner_note(output$summary_banner), cs[[4]])
      expect_identical(banner_counts(output$summary_banner)[["Conflicting trios"]],
                       paste0("1 id(s) (0 kept by you, 1 ", cs[[3]]))
      expect_match(ui_html(output$results_ui),
                   if (cs[[1]]) "Ids without a checked row are\\s+set to missing \\(0\\)\\." else
                     "Ids without a checked row are\\s+only flagged\\.")
    }
  })
})

test_that("a clean pedigree gets a green summary and empty sections", {
  ped <- upload_text(c("id\tmale_parent\tfemale_parent", "A\t0\t0", "B\t0\t0", "C\tA\tB"), "clean.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)
    expect_identical(banner_headline(output$summary_banner), "No issues found. Pedigree looks clean!")
    expect_identical(banner_counts(output$summary_banner),
                     c("Exact duplicates" = "0", "Conflicting trios" = "0 id(s)", "Inconsistent sex roles" = "0",
                       "Missing parents" = "0", "Cycles / dependencies" = "0"))
    expect_match(ui_html(output$summary_banner), "background-color:#d4edda", fixed = TRUE)
    ui <- output$results_ui
    expect_true(all(section_counts(ui) == "None found"))
    expect_length(section_counts(ui), 5)
    expect_false(grepl(session$ns("apply_keep"), ui_html(ui), fixed = TRUE))
  })
})

test_that("an id typed with a trailing space is the same individual, so its conflict is found", {
  ped <- upload_text(c("id\tfemale_parent\tmale_parent", "testid\tsire10\tdam1", "testid \tsire1\tdam2"),
                     "pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    expect_identical(tiles(output$dashboard)[["Unique IDs"]], "1")
    session$setInputs(run_check = 1)
    expect_identical(unique(check_results()$report$conflicting_trios$id), "testid")
  })
})

test_that("the downloads give the results workbook and the cleaned pedigree", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)
    res <- check_results()

    xlsx <- output$download_results
    expect_identical(basename(xlsx), paste0("pedigree_check_results_", Sys.Date(), ".xlsx"))
    expect_identical(openxlsx::getSheetNames(xlsx),
                     c("All Records", "Exact Duplicates", "Conflicting Trios", "Inconsistent Sex Roles",
                       "Missing Parents", "Cycles - Dependencies", "Settings"))

    rec <- read_sheet(xlsx, "All Records")
    expect_identical(names(rec), c("row", "id", "male_parent", "female_parent", "issue", "action",
                                   "cleaned_male_parent", "cleaned_female_parent"))
    expect_equal(rec, plain(cleaner_records(res$ped_input, res$report)), ignore_attr = TRUE)
    expect_identical(nrow(rec), 50L)   # 49 uploaded rows and M4
    expect_identical(rec$action[rec$id == "C02"], c("kept", "row removed (duplicate)"))
    expect_identical(rec$action[rec$id == "C07"], rep("disagreeing parent(s) set to missing, rows merged", 2))
    expect_identical(rec$cleaned_male_parent[rec$id == "C07"], c("0", "0"))
    expect_identical(rec$action[rec$id == "C09"], "male parent set to missing")
    expect_identical(rec$action[rec$id == "C10"], "female parent set to missing")
    expect_identical(rec$action[rec$id %in% c("X1", "X2")], rep("flagged, fix by hand", 2))
    expect_identical(rec$action[rec$id == "M4"], "added with unknown parents (0)")
    expect_true(is.na(rec$row[rec$id == "M4"]))
    others <- !rec$id %in% c("C02", "C07", "C09", "C10", "X1", "X2", "M4")
    expect_identical(sum(others), 41L)
    expect_true(all(rec$issue[others] == "none"))
    expect_true(all(rec$action[others] == "kept"))

    trios <- read_sheet(xlsx, "Conflicting Trios")
    expect_identical(names(trios), c("row_number", "id", "male_parent", "female_parent", "kept_by_user"))
    expect_identical(as.logical(trios$kept_by_user), c(FALSE, FALSE))
    expect_identical(read_sheet(xlsx, "Missing Parents")$id, "M4")
    expect_identical(read_sheet(xlsx, "Cycles - Dependencies")$dependency,
                     "Cycle detected involving IDs: X1 -> X2 -> X1")

    s <- settings_of(xlsx)
    expect_identical(names(s), c("Date", "Familia version", "BIGpopA version", "Tab", "Pedigree file",
                                 "Set conflicting parents to missing (0)",
                                 "Set inconsistent sex roles to missing (0)",
                                 "Rows kept by user (id: male x female)", "Always applied"))
    expect_identical(unname(s[-(1:3)]),
                     c("Pedigree Cleaner", "example_pedigree.txt", "TRUE", "TRUE", "none",
                       "exact duplicates removed; missing parents added with unknown parents (0)"))
    expect_identical(s[["Familia version"]], as.character(utils::packageVersion("Familia")))

    txt <- output$download_pedigree
    expect_identical(basename(txt), "pedigree_cleaned.txt")
    expect_identical(readLines(txt), readLines(extdata("example_pedigree_cleaned.txt")))
  })
})

test_that("keeping one row of a conflicting trio runs the check again without the other row", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)
    base     <- check_results()$base_report
    uploaded <- read_table_upload(ped)

    # rows of the Conflicting Trios table: 1 = C07 M2 x F3, 2 = C07 M1 x F3
    session$setInputs(tbl_conflicting_trios_rows_selected = 2L, apply_keep = 1)
    expect_identical(plain(kept_rows()), data.frame(id = "C07", male_parent = "M1", female_parent = "F3"))
    res      <- check_results()
    expected <- BIGpopA::check_ped(uploaded[!(uploaded$id == "C07" & uploaded$male_parent == "M2"), ],
                                   verbose = FALSE)
    expect_identical(res$base_report, base)
    expect_identical(res$report, expected)
    expect_identical(nrow(res$report$conflicting_trios), 0L)
    cleaned <- plain(res$report$corrected_pedigree)
    expect_identical(unlist(cleaned[cleaned$id == "C07", ], use.names = FALSE), c("C07", "M1", "F3"))
    expect_identical(banner_counts(output$summary_banner),
                     replace(example_counts, "Conflicting trios", "1 id(s) (1 kept by you, 0 set to missing)"))
    expect_identical(section_counts(output$results_ui)[["Conflicting Trios"]], "2 record(s) found")
    expect_length(log$notes, 0)

    # downloads follow the kept row
    xlsx <- output$download_results
    expect_identical(as.logical(read_sheet(xlsx, "Conflicting Trios")$kept_by_user), c(FALSE, TRUE))
    rec <- read_sheet(xlsx, "All Records")
    expect_identical(rec$action[rec$id == "C07"], c("row removed (another row kept by user)", "kept by user"))
    expect_identical(rec$cleaned_male_parent[rec$id == "C07"], c("M1", "M1"))
    expect_identical(settings_of(xlsx)[["Rows kept by user (id: male x female)"]], "C07: M1 x F3")
    txt <- readLines(output$download_pedigree)
    expect_identical(txt, table_lines(expected$corrected_pedigree))
    expect_true("C07\tM1\tF3" %in% txt)

    # Reset goes back to the run on the uploaded pedigree
    session$setInputs(reset_keep = 1)
    expect_null(kept_rows())
    expect_identical(check_results()$report, base)
    expect_identical(banner_counts(output$summary_banner), example_counts)
  })
})

test_that("at most one row per id can be kept, and an empty selection keeps nothing", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(!!!cleaner_defaults, ped_file = ped)
    session$setInputs(run_check = 1)
    base <- check_results()$base_report

    session$setInputs(tbl_conflicting_trios_rows_selected = c(1L, 2L), apply_keep = 1)
    expect_identical(log$notes,
                     "Keep at most one row per id. More than one row is checked for: C07. Nothing was applied.")
    expect_null(kept_rows())
    expect_identical(check_results()$report, base)

    session$setInputs(tbl_conflicting_trios_rows_selected = 1L, apply_keep = 2)
    expect_identical(kept_rows()$male_parent, "M2")
    session$setInputs(tbl_conflicting_trios_rows_selected = NULL, apply_keep = 3)
    expect_null(kept_rows())
    expect_identical(check_results()$report, base)

    # a new run starts without kept rows
    session$setInputs(tbl_conflicting_trios_rows_selected = 2L, apply_keep = 4)
    expect_false(is.null(kept_rows()))
    session$setInputs(run_check = 2)
    expect_null(kept_rows())
    expect_identical(check_results()$report, base)
  })
})

test_that("a kept row is applied with the options of the run", {
  ped <- upload_example("example_pedigree.txt")
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    session$setInputs(ped_file = ped, correct_conflicting_trios = FALSE, correct_inconsistent_sex_roles = FALSE)
    session$setInputs(run_check = 1)
    session$setInputs(tbl_conflicting_trios_rows_selected = 1L, apply_keep = 1)
    uploaded <- read_table_upload(ped)
    expect_identical(check_results()$report,
                     BIGpopA::check_ped(uploaded[!(uploaded$id == "C07" & uploaded$male_parent == "M1"), ],
                                        verbose = FALSE, correct_conflicting_trios = FALSE,
                                        correct_inconsistent_sex_roles = FALSE))
    expect_identical(banner_counts(output$summary_banner)[["Conflicting trios"]],
                     "1 id(s) (1 kept by you, 0 flagged)")
  })
})

test_that("Help opens the Pedigree Cleaner help and the example link gives the example pedigree", {
  shiny::testServer(mod_ped_cleaner_server, args = cleaner_args, {
    log <- record_session(session)
    session$setInputs(help_btn = 1)
    expect_length(log$modals, 1)
    expect_match(log$modals, "Pedigree Cleaner - Help", fixed = TRUE)
    expect_match(log$modals, 'id="modal_pc_help_conflict"', fixed = TRUE)

    path <- output$download_examples
    expect_identical(basename(path), "example_pedigree.txt")
    expect_identical(readLines(path), readLines(extdata("example_pedigree.txt")))
  })
})

test_that("sanitize_id() and make_collapse_panel() build the ids the tables and help use", {
  expect_identical(sanitize_id(c("Exact Duplicates", "Cycles / Dependencies", "No Genotype Data")),
                   c("exact_duplicates", "cycles___dependencies", "no_genotype_data"))
  html <- ui_html(make_collapse_panel("p1", "copy", "Label", shiny::p("Body")))
  expect_match(html, 'data-target="#p1"', fixed = TRUE)
  expect_match(open_tag(html, "p1"), 'class="collapse"', fixed = TRUE)
  expect_match(html, "<span>Label</span>", fixed = TRUE)
  expect_match(html, "<p>Body</p>", fixed = TRUE)
})
