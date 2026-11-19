testthat::local_edition(3)

# Strict tests of the shared helpers in R/utils_results.R: the exact value each
# one returns for the cases the tabs give it.

#### read_table_upload() ####

test_that("read_table_upload() reads every cell as text, trimmed, with # and quotes as typed", {
  up  <- upload_text(c("id\tmale_parent\tfemale_parent\tnote",
                       "001\t0\t0\tA#1",
                       "  002 \t 001\t\t\"x y\"",
                       "NA\tNA\t0\t"), "pedigree.txt")
  ped <- read_table_upload(up)
  expect_s3_class(ped, "data.frame")
  expect_identical(names(ped), c("id", "male_parent", "female_parent", "note"))
  expect_identical(ped$id,            c("001", "002", NA))   # leading zeros kept, spaces removed
  expect_identical(ped$male_parent,   c("0", "001", NA))     # "NA" is missing
  expect_identical(ped$female_parent, c("0", "", "0"))       # an empty cell stays empty
  expect_identical(ped$note,          c("A#1", "x y", ""))   # # is not a comment
})

test_that("read_table_upload() reads .csv (any case) as comma-separated, quoted cells trimmed too", {
  csv <- read_table_upload(upload_text(c("ID,Male Parent,female.parent",
                                         "\"A, 1\",\" B \",C",
                                         "D,E"), "PEDIGREE.CSV"))
  expect_identical(names(csv), c("ID", "Male Parent", "female.parent"))   # names kept as typed
  expect_identical(csv$ID, c("A, 1", "D"))
  expect_identical(csv[["Male Parent"]], c("B", "E"))
  expect_identical(csv$female.parent, c("C", ""))                          # short row filled
})

test_that("read_table_upload() reads .txt and .tsv as tab-separated and stops on a short row", {
  lines <- c("id\tmale_parent\tfemale_parent", "A\t0\t0")
  expect_identical(read_table_upload(upload_text(lines, "p.tsv")),
                   read_table_upload(upload_text(lines, "p.txt")))
  expect_error(read_table_upload(upload_text(c(lines, "B\t0"), "p.txt")), "did not have 3 elements")
})

#### results_datatable() ####

test_that("results_datatable() adds a filter box per column, highlights matches and has no row names", {
  w <- results_datatable(data.frame(id = c("a", "b"), n = 1:2))
  expect_s3_class(w, "datatables")
  expect_identical(w$x$filter, "top")
  expect_identical(w$x$options$pageLength, 10)
  expect_true(w$x$options$scrollX)
  expect_true(w$x$options$searchHighlight)
  header_cells <- regmatches(w$x$container, gregexpr("<th>", w$x$container, fixed = TRUE))[[1]]
  expect_length(header_cells, 2)   # id and n, no row-name column
})

test_that("results_datatable() passes the page length, extra options and selection through", {
  df <- data.frame(id = c("a", "b"), n = 1:2)
  expect_identical(results_datatable(df, page_length = 25)$x$options$pageLength, 25)

  w <- results_datatable(df, page_length = 25, options = list(pageLength = 5, dom = "t"))
  expect_identical(w$x$options$pageLength, 5)
  expect_identical(w$x$options$dom, "t")
  expect_true(w$x$options$searchHighlight)

  w <- results_datatable(df, selection = list(mode = "multiple", selected = 2L),
                         options = list(columnDefs = list(list(targets = 0, className = "keep-cell"))))
  expect_identical(w$x$selection$mode, "multiple")
  expect_identical(w$x$selection$selected, 2L)
  keep_def <- vapply(w$x$options$columnDefs,
                     function(d) identical(d$className, "keep-cell") && identical(d$targets, 0), logical(1))
  expect_true(any(keep_def))

  expect_s3_class(results_datatable(data.table::data.table(id = "a")), "datatables")
})

#### summary tiles ####

test_that("summary_strip() shows each count with thousands separators, a dash when missing, and the note", {
  ui <- summary_strip(list("Records" = 1234567L, "Share" = 0.5, "Lines" = "2 / 3", "Markers" = NA,
                           "Empty" = NULL, "Big" = 1e6, "Both NA" = c(NA, NA)),
                      note = "Could not read the file: x")
  expect_identical(tiles(ui), c("Records" = "1,234,567", "Share" = "0.5", "Lines" = "2 / 3",
                                "Markers" = DASH, "Empty" = DASH, "Big" = "1,000,000", "Both NA" = DASH))
  expect_identical(tiles_note(ui), "Could not read the file: x")
  html <- ui_html(ui)
  expect_match(html, '<div class="summary-strip">', fixed = TRUE)
  expect_length(regmatches(html, gregexpr('class="summary-tile"', html, fixed = TRUE))[[1]], 7)

  expect_identical(tiles_note(summary_strip(list("A" = 1L))), NA_character_)   # no note, no <p>
})

test_that("format_pct() gives one decimal and NA when there is no value", {
  expect_identical(format_pct(0.0123), "1.2%")
  expect_identical(format_pct(0),      "0.0%")
  expect_identical(format_pct(0.25),   "25.0%")
  expect_identical(format_pct(1),      "100.0%")
  expect_identical(format_pct(NA),     NA)
  expect_identical(format_pct(numeric(0)), NA)
})

#### pedigree columns and counts ####

test_that("pedigree_columns() matches id and parent columns ignoring case, spaces, dots and dashes", {
  expect_identical(pedigree_columns(data.frame(ID = 1, `Male Parent` = 1, female.parent = 1, extra = 1,
                                               check.names = FALSE)),
                   c(id = "ID", male_parent = "Male Parent", female_parent = "female.parent"))
  expect_identical(pedigree_columns(data.frame(` id ` = 1, `MALE-PARENT` = 1, Female_Parent = 1,
                                               check.names = FALSE)),
                   c(id = " id ", male_parent = "MALE-PARENT", female_parent = "Female_Parent"))
  expect_identical(pedigree_columns(data.frame(id = 1, ID = 1, male_parent = 1, female_parent = 1,
                                               check.names = FALSE))[["id"]], "id")   # first match
  expect_null(pedigree_columns(data.frame(id = 1, sire = 1, dam = 1)))
  expect_null(pedigree_columns(data.frame(id = 1, maleparent = 1, femaleparent = 1)))
})

test_that("known_parent() treats 0, NA and empty cells as unknown", {
  expect_identical(known_parent(c("A", "0", "", NA, "NA", " 0 ", " ", "na")),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(known_parent(c(0, 1)), c(FALSE, TRUE))
})

test_that("pedigree_counts() gives the five tile counts as integers", {
  ped <- data.frame(ID = c("A", "B", "C", "D", "D"), `Male Parent` = c("0", "0", "A", "A", "NA"),
                    female.parent = c("0", NA, "B", "", "B"), check.names = FALSE)
  expect_identical(pedigree_counts(ped),
                   list("Records" = 5L, "Unique IDs" = 4L, "Complete trios" = 1L,
                        "One parent known" = 2L, "Both parents unknown" = 2L))
  expect_null(pedigree_counts(data.frame(id = "A", sire = "0")))
})

test_that("testable_trios() counts complete trios whose three members are genotyped", {
  ped <- data.frame(id = c("A", "B", "C", "D", "E"), male_parent = c("0", "0", "A", "A", "A"),
                    female_parent = c("0", "0", "B", "0", "X"))
  expect_identical(testable_trios(ped, c("A", "B", "C", "D", "E")), 1L)   # D has one parent, X not genotyped
  expect_identical(testable_trios(ped, c("A", "C")), 0L)
  expect_identical(testable_trios(data.frame(id = "A"), "A"), NA_integer_)
})

test_that("genotype_counts() finds the ID column in any case, or uses the first column", {
  gc <- genotype_counts(data.frame(Id = c("A", "B"), m1 = c(0, NA), m2 = c(1, 2), m3 = c(NA, NA)))
  expect_identical(gc$ids, c("A", "B"))
  expect_identical(gc$markers, 3L)
  expect_identical(gc$missing, 0.5)

  gc <- genotype_counts(data.frame(sample = c(10, 11), m1 = c(0, 1)))
  expect_identical(gc$ids, c("10", "11"))   # first column, as text
  expect_identical(gc$markers, 1L)
  expect_identical(gc$missing, 0)

  gc <- genotype_counts(data.frame(id = c("A", "B")))
  expect_identical(gc$markers, 0L)
  expect_identical(gc$missing, NA_real_)
})

#### recommended actions ####

test_that("every recommended correction code has its plain-language label", {
  codes <- c("none", "keep_both", "remove_male_parent", "remove_female_parent", "remove_both",
             "unresolved", "low_markers_keep_both", "low_markers_remove_male_parent",
             "low_markers_remove_female_parent", "low_markers_remove_both", "low_markers_unresolved")
  expect_identical(recommended_action_label(codes), c(
    "None", "Keep both parents", "Set male parent to missing", "Set female parent to missing",
    "Set both parents to missing", "Unresolved: review by hand", "Low markers: keep both parents",
    "Low markers: set male parent to missing", "Low markers: set female parent to missing",
    "Low markers: set both parents to missing", "Low markers: unresolved, review by hand"
  ))
  expect_identical(recommended_action_label(c("new_code", NA)), c("new_code", NA))
  expect_identical(recommended_action_label(factor("remove_both")), "Set both parents to missing")
})

test_that("plain_actions() renames recommended_correction in place and leaves other tables alone", {
  dt  <- data.table::data.table(id = "a", recommended_correction = "remove_male_parent", status = "fail")
  out <- plain_actions(dt)
  expect_identical(class(out), "data.frame")
  expect_identical(names(out), c("id", "recommended_action", "status"))
  expect_identical(out$recommended_action, "Set male parent to missing")
  expect_identical(out$status, "fail")

  other <- data.frame(marker = "snp1", mismatch_pct = 1.5)
  expect_identical(plain_actions(other), other)
})

#### All Records of the Pedigree Cleaner ####

# One case of each issue: C duplicated, D with two male parents, G with a
# parent without a row (M9), X1 <-> X2 a cycle, P and Q used as both sexes
cleaner_ped <- data.frame(
  id            = c("A", "B", "C", "C", "D", "D", "G",  "X1", "X2", "P", "Q", "K1", "K2"),
  male_parent   = c("0", "0", "A", "A", "A", "E", "M9", "X2", "X1", "0", "0", "P",  "Q"),
  female_parent = c("0", "0", "B", "B", "B", "B", "B",  "0",  "0",  "0", "0", "Q",  "P"),
  stringsAsFactors = FALSE
)
sex_issue  <- "male parent has inconsistent sex role; female parent has inconsistent sex role"
sex_action <- "male parent set to missing; female parent set to missing"

test_that("cleaner_records() gives every record its issue, action and cleaned parents", {
  report <- BIGpopA::check_ped(cleaner_ped, verbose = FALSE)
  expected <- data.frame(
    row           = c(1:13, NA),
    id            = c(cleaner_ped$id, "M9"),
    male_parent   = c(cleaner_ped$male_parent, "0"),
    female_parent = c(cleaner_ped$female_parent, "0"),
    issue  = c("none", "none", "exact duplicate", "exact duplicate", "conflicting trio", "conflicting trio",
               "none", "cycle or dependency", "cycle or dependency", "none", "none", sex_issue, sex_issue,
               "listed as a parent but has no row"),
    action = c("kept", "kept", "kept", "row removed (duplicate)",
               rep("disagreeing parent(s) set to missing, rows merged", 2), "kept",
               rep("flagged, fix by hand", 2), "kept", "kept", sex_action, sex_action,
               "added with unknown parents (0)"),
    cleaned_male_parent   = c("0", "0", "A", "A", "0", "0", "M9", "X2", "X1", "0", "0", "0", "0", "0"),
    cleaned_female_parent = c("0", "0", "B", "B", "B", "B", "B",  "0",  "0",  "0", "0", "0", "0", "0"),
    stringsAsFactors = FALSE
  )
  expect_identical(plain(cleaner_records(cleaner_ped, report)), expected)
})

test_that("cleaner_records() reports flagged issues as not changed when the options are off", {
  report  <- BIGpopA::check_ped(cleaner_ped, verbose = FALSE, correct_conflicting_trios = FALSE,
                                correct_inconsistent_sex_roles = FALSE)
  records <- cleaner_records(cleaner_ped, report, set_conflicts = FALSE, set_sex = FALSE)
  expect_identical(records$id, c(cleaner_ped$id, "E", "M9"))   # E is a parent without a row too
  expect_identical(records$action, c(
    "kept", "kept", "kept", "row removed (duplicate)", "flagged, not changed", "flagged, not changed",
    "kept", "flagged, fix by hand", "flagged, fix by hand", "kept", "kept",
    "flagged, not changed", "flagged, not changed",
    "added with unknown parents (0)", "added with unknown parents (0)"
  ))
  expect_identical(records$issue[12:13], c(sex_issue, sex_issue))
  unique_rows <- -c(5, 6)   # D keeps both rows in the cleaned pedigree
  expect_identical(records$cleaned_male_parent[unique_rows],
                   c("0", "0", "A", "A", "M9", "X2", "X1", "0", "0", "P", "Q", "0", "0"))
  expect_identical(records$cleaned_female_parent[unique_rows],
                   c("0", "0", "B", "B", "B", "0", "0", "0", "0", "Q", "P", "0", "0"))
})

test_that("cleaner_records() marks the row kept by the user and the row removed for it", {
  report   <- BIGpopA::check_ped(cleaner_ped, verbose = FALSE)
  report_k <- BIGpopA::check_ped(cleaner_ped[-5, ], verbose = FALSE)   # D: E x B kept, A x B removed
  kept     <- data.frame(id = "D", male_parent = "E", female_parent = "B", stringsAsFactors = FALSE)
  expected <- data.frame(
    row           = c(1:13, NA, NA),
    id            = c(cleaner_ped$id, "E", "M9"),
    male_parent   = c(cleaner_ped$male_parent, "0", "0"),
    female_parent = c(cleaner_ped$female_parent, "0", "0"),
    issue  = c("none", "none", "exact duplicate", "exact duplicate", "conflicting trio", "conflicting trio",
               "none", "cycle or dependency", "cycle or dependency", "none", "none", sex_issue, sex_issue,
               rep("listed as a parent but has no row", 2)),
    action = c("kept", "kept", "kept", "row removed (duplicate)", "row removed (another row kept by user)",
               "kept by user", "kept", rep("flagged, fix by hand", 2), "kept", "kept", sex_action, sex_action,
               rep("added with unknown parents (0)", 2)),
    cleaned_male_parent   = c("0", "0", "A", "A", "E", "E", "M9", "X2", "X1", "0", "0", "0", "0", "0", "0"),
    cleaned_female_parent = c("0", "0", "B", "B", "B", "B", "B",  "0",  "0",  "0", "0", "0", "0", "0", "0"),
    stringsAsFactors = FALSE
  )
  expect_identical(plain(cleaner_records(cleaner_ped, report_k, conflicts = report$conflicting_trios,
                                         kept = kept)), expected)
})

#### issue columns of the downloads ####

test_that("insert_after() puts the new column right after another one, or last", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_identical(names(insert_after(df, "a", "x", "new")), c("a", "x", "b", "c"))
  expect_identical(names(insert_after(df, "c", "x", "new")), c("a", "b", "c", "x"))
  expect_identical(names(insert_after(df, "zz", "x", "new")), c("a", "b", "c", "x"))
  expect_identical(insert_after(df, "a", "x", "new")$x, "new")
  expect_identical(names(insert_after(data.frame(a = 1), "a", "x", 2)), c("a", "x"))
})

test_that("validation_issues() states the issue of every status and the action in plain language", {
  vr <- data.table::data.table(
    id                          = c("p", "f", "l", "g", "b", "m", "w", "n", "o"),
    orig_male_parent            = c("M", "M", "M", "M", "0", "0", "M", "M", "0"),
    orig_female_parent          = c("F", "F", "F", "F", "0", "F", "0", "F", "0"),
    trio_mendelian_error_pct    = c(0, 41.666, 0, NA, NA, NA, NA, NA, NA),
    trio_markers_tested         = c(12L, 12L, 4L, 0L, 0L, 0L, 0L, 0L, 0L),
    status                      = c("pass", "fail", "low_markers", "no_genotype_data",
                                    "missing_both_parents", "missing_male_parent",
                                    "missing_female_parent", "no_data", "founders"),
    recommended_correction      = c("none", "remove_both", "low_markers_remove_male_parent",
                                    rep("none", 6)),
    male_parent_hom_error_pct   = c(NA, 10, 5, rep(NA, 6)),
    female_parent_hom_error_pct = NA_real_
  )
  out <- validation_issues(vr, 5, 10)
  expect_identical(class(out), "data.frame")
  expect_identical(names(out), c("id", "orig_male_parent", "orig_female_parent", "trio_mendelian_error_pct",
                                 "trio_markers_tested", "status", "issue", "recommended_correction",
                                 "recommended_action", "male_parent_hom_error_pct",
                                 "female_parent_hom_error_pct"))
  expect_identical(out$issue, c("none", "fail: trio error 41.67% > 5%", "low markers: 4 testable markers < 10",
                                "no genotype data for the offspring or a parent", "both parents unknown",
                                "male parent unknown", "female parent unknown", "no testable marker", "none"))
  expect_identical(out$recommended_correction, vr$recommended_correction)   # codes kept
  expect_identical(out$recommended_action[1:3], c("None", "Set both parents to missing",
                                                   "Low markers: set male parent to missing"))
  expect_identical(out$trio_mendelian_error_pct, vr$trio_mendelian_error_pct)

  expect_identical(validation_issues(vr, 5.5, 8)$issue[2:3],
                   c("fail: trio error 41.67% > 5.5%", "low markers: 4 testable markers < 8"))
  no_code <- as.data.frame(vr)[names(vr) != "recommended_correction"]
  expect_false("recommended_action" %in% names(validation_issues(no_code, 5, 10)))
})

test_that("parentage_issues() states the issue of every status and the ties", {
  pr <- data.frame(id = c("a", "b", "c", "d", "e"), male_parent = "M", female_parent = "F",
                   mendelian_error_pct = c("0.00", "12.50", "0.00", NA, "1.00"),
                   markers_tested = c(100L, 100L, 8L, 0L, 50L),
                   status = c("pass", "high_error", "low_markers", "known_parent_no_genotype", "pass"),
                   tied_candidates = c(1L, 2L, NA, NA, 3L))
  out <- parentage_issues(pr, 5, 10)
  expect_identical(names(out), c("id", "male_parent", "female_parent", "mendelian_error_pct",
                                 "markers_tested", "status", "issue", "tied_candidates"))
  expect_identical(out$issue, c("none", "high error: best error 12.50% > 5%; 2 tied candidates",
                                "low markers: 8 markers tested < 10", "known parent has no genotype",
                                "3 tied candidates"))
  expect_identical(parentage_issues(pr[, names(pr) != "tied_candidates"], 5, 10)$issue,
                   c("none", "high error: best error 12.50% > 5%", "low markers: 8 markers tested < 10",
                     "known parent has no genotype", "none"))
})

#### clickable Mendelian error plots ####

test_that("a click on a bar gives exactly the rows ggplot2 counted in that bar", {
  d <- data.frame(id = paste0("t", 1:6), err = c(0, 0.5, 0.51, 1.5, 3.2, NA), plot_status = "pass",
                  stringsAsFactors = FALSE)
  p  <- ggplot2::ggplot(d[!is.na(d$err), ], ggplot2::aes(x = err)) +
    ggplot2::geom_histogram(binwidth = 1)
  br <- histogram_breaks(p)
  expect_identical(br, c(-0.5, 0.5, 1.5, 2.5, 3.5))

  bar1 <- histogram_bar_rows(d, br, 0, "err")
  expect_identical(bar1$label, "0 to 0.5%")
  expect_identical(bar1$rows$id, c("t1", "t2"))                   # 0.5 is in the first bar (closed on the right)
  expect_identical(names(bar1$rows), c("id", "err"))               # plot_status is dropped
  expect_identical(histogram_bar_rows(d, br, 0.5, "err"), bar1)
  expect_identical(histogram_bar_rows(d, br, 0.51, "err")$rows$id, c("t3", "t4"))
  expect_identical(histogram_bar_rows(d, br, 0.51, "err")$label, "0.5 to 1.5%")
  expect_identical(histogram_bar_rows(d, br, 3.4, "err")$label, "2.5 to 3.5%")

  # same counts as the bars drawn
  ld <- ggplot2::layer_data(p)
  for (x in c(0, 1, 3)) {
    expect_identical(nrow(histogram_bar_rows(d, br, x, "err")$rows),
                     as.integer(sum(ld$count[ld$xmin < x & x <= ld$xmax])))
  }

  expect_null(histogram_bar_rows(d, br, 2, "err"))      # empty bar
  expect_null(histogram_bar_rows(d, br, -1, "err"))     # left of the plot
  expect_null(histogram_bar_rows(d, br, 4, "err"))      # right of the plot
  expect_null(histogram_bar_rows(d, br, NULL, "err"))
  expect_null(histogram_bar_rows(d, br, NA_real_, "err"))
  expect_null(histogram_bar_rows(d, br, Inf, "err"))
  expect_null(histogram_bar_rows(NULL, br, 0, "err"))
  expect_null(histogram_bar_rows(d, 0.5, 0, "err"))
})

test_that("bar rows are found in text columns and data.tables (Find Parentage errors are text)", {
  d  <- data.table::data.table(id = c("a", "b"), mendelian_error_pct = c("0.00", "2.10"))
  br <- c(-0.5, 0.5, 1.5, 2.5)
  expect_identical(histogram_bar_rows(d, br, 2, "mendelian_error_pct")$rows$id, "b")
  expect_s3_class(histogram_bar_rows(d, br, 2, "mendelian_error_pct")$rows, "data.frame")
})

#### downloads ####

test_that("settings_table() lists the date, both versions and every setting as text", {
  s <- settings_table("Tab" = "X", "Ploidy" = 4L, "Flag" = TRUE, "Files" = c("a.txt", "b.txt"),
                      "None" = NULL, "Empty" = character(0), "Threshold" = 5.5)
  expect_identical(names(s), c("setting", "value"))
  expect_identical(s$setting, c("Date", "Familia version", "BIGpopA version", "Tab", "Ploidy", "Flag",
                                "Files", "None", "Empty", "Threshold"))
  expect_match(s$value[1], "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$")
  expect_identical(s$value[-1], c(as.character(utils::packageVersion("Familia")),
                                  as.character(utils::packageVersion("BIGpopA")),
                                  "X", "4", "TRUE", "a.txt, b.txt", "", "", "5.5"))
  expect_identical(rownames(s), as.character(seq_len(nrow(s))))
})

test_that("write_results_xlsx() writes one sheet per table, in order, with notes for empty tables", {
  f    <- withr::local_tempfile(fileext = ".xlsx")
  data <- data.frame(id = c("a", "b"), n = 1:2, flag = c(TRUE, FALSE), x = c(1.5, NA))
  out  <- expect_invisible(write_results_xlsx(list(
    "All Records"                          = data,
    "No rows"                              = data.frame(a = integer(0)),
    "Nothing"                              = data.frame(),
    "Missing"                              = NULL,
    "A/B\\C?D*E:F"                         = data,
    "A sheet name longer than thirty-one"  = data,
    "Wide"                                 = as.data.frame(matrix(0L, nrow = 1, ncol = 16385))
  ), f))
  expect_identical(out, f)
  expect_identical(openxlsx::getSheetNames(f),
                   c("All Records", "No rows", "Nothing", "Missing", "A-B-C-D-E-F",
                     "A sheet name longer than thirty", "Wide"))
  back <- read_sheet(f, "All Records")
  expect_identical(names(back), c("id", "n", "flag", "x"))
  expect_identical(back$id, c("a", "b"))
  expect_equal(back$n, c(1, 2))
  expect_identical(as.logical(back$flag), c(TRUE, FALSE))
  expect_equal(back$x, c(1.5, NA))
  for (sheet in c("No rows", "Nothing", "Missing")) {
    note <- read_sheet(f, sheet)
    expect_identical(names(note), "note")
    expect_identical(note$note, "None found")
  }
  expect_identical(read_sheet(f, "Wide")$note, "Too large for an Excel sheet: see the table in the app.")
})
