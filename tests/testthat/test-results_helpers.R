# Unit tests for the shared result helpers (R/utils_results.R) and the
# read-once genotype helpers (R/utils.R).

# Shiny fileInput value for some lines of text (renamed datapath, no extension)
upload_lines <- function(lines, name, env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = env)
  writeLines(lines, path)
  list(name = name, datapath = path)
}

tetra_vcf <- c(
  "##fileformat=VCFv4.3",
  "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
  paste("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO",
        "FORMAT", "A", "B", "C", sep = "\t"),
  paste("chr1", "100", "snp1", "A", "G", ".", ".", ".", "GT", "0/0/0/0", "0/1/1/1", "1/1/1/1", sep = "\t"),
  paste("chr1", "200", "snp2", "C", "T", ".", ".", ".", "GT", "0/0/1/1", "./././.", "0/0/0/1", sep = "\t")
)

test_that("read_table_upload() reads tab and comma files as text", {
  txt <- read_table_upload(upload_lines(c("id\tsex", "001\tM", "002\tF"), "parents.txt"))
  csv <- read_table_upload(upload_lines(c("id,sex", "001,M", "002,F"), "parents.csv"))
  expect_identical(txt$id, c("001", "002"))   # leading zeros kept
  expect_identical(csv, txt)
})

test_that("spaces around an id do not hide a conflicting trio", {
  # the second testid has a trailing space, as in a pedigree typed by hand
  ped <- read_table_upload(upload_lines(c("id\tfemale_parent\tmale_parent",
                                          "testid\tsire10\tdam1",
                                          "testid \tsire1\tdam2"), "pedigree.txt"))
  expect_identical(ped$id, c("testid", "testid"))
  expect_identical(pedigree_counts(ped)[["Unique IDs"]], 1L)
  report <- BIGpopA::check_ped(ped, verbose = FALSE)
  expect_identical(unique(report$conflicting_trios$id), "testid")

  # quoted cells too, as written by write.csv()
  csv <- read_table_upload(upload_lines(c("id,female_parent,male_parent",
                                          "\"testid\",\"sire10\",\"dam1\"",
                                          "\"testid \",\" sire1\",\"dam2\""), "pedigree.csv"))
  expect_identical(csv$id, c("testid", "testid"))
  expect_identical(csv$female_parent, c("sire10", "sire1"))

  # and the id column of a genotype text file
  geno <- read_genotype_upload(upload_lines(c("id,m1", "\"testid \",1"), "geno.csv"), ploidy = 2)
  expect_identical(geno$id, "testid")
})

test_that("pedigree counts match column names loosely and treat 0, NA and empty as unknown", {
  ped <- data.frame(ID = c("A", "B", "C", "D"), `Male Parent` = c("0", "0", "A", "A"),
                    female.parent = c("0", NA, "B", ""), check.names = FALSE)
  counts <- pedigree_counts(ped)
  expect_identical(counts[["Records"]], 4L)
  expect_identical(counts[["Unique IDs"]], 4L)
  expect_identical(counts[["Complete trios"]], 1L)
  expect_identical(counts[["One parent known"]], 1L)
  expect_identical(counts[["Both parents unknown"]], 2L)
  expect_null(pedigree_counts(data.frame(id = "A")))

  expect_identical(testable_trios(ped, c("A", "B", "C")), 1L)
  expect_identical(testable_trios(ped, c("A", "C")), 0L)
})

test_that("genotype_counts() gives the ids, markers and share of missing calls", {
  gc <- genotype_counts(data.frame(id = c("A", "B"), m1 = c(0, NA), m2 = c(1, 2)))
  expect_identical(gc$ids, c("A", "B"))
  expect_identical(gc$markers, 2L)
  expect_equal(gc$missing, 0.25)
})

test_that("summary tiles show the counts, and a dash before upload", {
  html <- as.character(summary_strip(list("Records" = 1234L, "Markers" = NA), note = "check"))
  expect_true(grepl("1,234", html, fixed = TRUE))
  expect_true(grepl("—", html, fixed = TRUE))
  expect_true(grepl("check", html, fixed = TRUE))
  expect_identical(format_pct(0.0123), "1.2%")
})

test_that("recommended actions are shown in plain language, unknown codes kept", {
  expect_identical(
    recommended_action_label(c("remove_both", "low_markers_keep_both", "new_code", NA)),
    c("Set both parents to missing", "Low markers: keep both parents", "new_code", NA)
  )
  df <- plain_actions(data.frame(id = "a", recommended_correction = "remove_male_parent"))
  expect_identical(names(df), c("id", "recommended_action"))
  expect_identical(df$recommended_action, "Set male parent to missing")
})

test_that("cleaner_records() gives every record its issue and action", {
  ped <- data.frame(
    id            = c("A", "B", "F", "C", "C", "D", "D", "G",  "X1", "X2"),
    male_parent   = c("0", "0", "0", "A", "A", "A", "F", "M9", "X2", "X1"),
    female_parent = c("0", "0", "0", "B", "B", "B", "B", "B",  "0",  "0"),
    stringsAsFactors = FALSE
  )
  report  <- BIGpopA::check_ped(ped, verbose = FALSE)
  records <- cleaner_records(ped, report)

  expect_identical(nrow(records), nrow(ped) + 1L)                    # M9 added
  expect_identical(records$issue[1], "none")
  expect_identical(records$action[1], "kept")
  expect_identical(records$issue[4:5], rep("exact duplicate", 2))
  expect_identical(records$action[4:5], c("kept", "row removed (duplicate)"))
  expect_identical(records$issue[6:7], rep("conflicting trio", 2))
  expect_identical(records$action[6], "disagreeing parent(s) set to missing, rows merged")
  expect_identical(records$cleaned_male_parent[6], "0")
  expect_identical(records$issue[9], "cycle or dependency")
  expect_identical(records$action[9], "flagged, fix by hand")
  expect_identical(records$id[11], "M9")
  expect_identical(records$action[11], "added with unknown parents (0)")

  # Keeping the F x B row of D: the other row is removed and F is used
  kept     <- data.frame(id = "D", male_parent = "F", female_parent = "B", stringsAsFactors = FALSE)
  report_k <- BIGpopA::check_ped(ped[-6, ], verbose = FALSE)
  records  <- cleaner_records(ped, report_k, conflicts = report$conflicting_trios, kept = kept)
  expect_identical(records$action[6:7], c("row removed (another row kept by user)", "kept by user"))
  expect_identical(records$cleaned_male_parent[7], "F")
})

test_that("cleaner_records() reports inconsistent sex roles as set to missing or flagged", {
  ped <- data.frame(id = c("P", "Q", "K1", "K2"), male_parent = c("0", "0", "P", "Q"),
                    female_parent = c("0", "0", "Q", "P"), stringsAsFactors = FALSE)
  records <- cleaner_records(ped, BIGpopA::check_ped(ped, verbose = FALSE))
  expect_identical(records$issue[3],
                   "male parent has inconsistent sex role; female parent has inconsistent sex role")
  expect_identical(records$action[3], "male parent set to missing; female parent set to missing")

  flagged <- cleaner_records(ped, BIGpopA::check_ped(ped, verbose = FALSE,
                                                     correct_inconsistent_sex_roles = FALSE),
                             set_sex = FALSE)
  expect_identical(flagged$action[3], "flagged, not changed")
})

test_that("validation and parentage results get an issue column after status", {
  vr <- data.frame(
    id = c("a", "b", "c", "d"), status = c("pass", "fail", "low_markers", "missing_both_parents"),
    trio_mendelian_error_pct = c(0, 41.666, 0, NA), trio_markers_tested = c(12L, 12L, 4L, 0L),
    recommended_correction = c("none", "remove_male_parent", "low_markers_keep_both", "none"),
    stringsAsFactors = FALSE
  )
  out <- validation_issues(vr, 5, 10)
  expect_identical(out$issue, c("none", "fail: trio error 41.67% > 5%",
                                "low markers: 4 testable markers < 10", "both parents unknown"))
  expect_identical(names(out)[3], "issue")
  expect_identical(out$recommended_correction[2], "remove_male_parent")   # code kept
  expect_identical(out$recommended_action[2], "Set male parent to missing")

  pr <- data.frame(
    id = c("a", "b", "c"), mendelian_error_pct = c("0.00", "12.50", "0.00"),
    markers_tested = c(100L, 100L, 8L), status = c("pass", "high_error", "low_markers"),
    tied_candidates = c(2L, 1L, 1L), stringsAsFactors = FALSE
  )
  expect_identical(parentage_issues(pr, 5, 10)$issue,
                   c("2 tied candidates", "high error: best error 12.50% > 5%",
                     "low markers: 8 markers tested < 10"))
})

test_that("a click on a histogram bar gives the rows counted in that bar", {
  d  <- data.frame(id = paste0("t", 1:5), err = c(0, 0.2, 3.1, 3.4, 10), stringsAsFactors = FALSE)
  p  <- ggplot2::ggplot(d, ggplot2::aes(x = err)) + ggplot2::geom_histogram(binwidth = 1)
  br <- histogram_breaks(p)
  expect_identical(histogram_bar_rows(d, br, 3.2, "err")$rows$id, c("t3", "t4"))
  expect_identical(histogram_bar_rows(d, br, 0.1, "err")$rows$id, c("t1", "t2"))
  expect_null(histogram_bar_rows(d, br, 6, "err"))    # empty bar
  expect_null(histogram_bar_rows(d, br, 50, "err"))   # outside the plot
})

test_that("results are written as one workbook, empty tables get a note", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_results_xlsx(list("All Records" = data.frame(a = 1:2), "Empty" = data.frame(),
                          "Cycles / Dependencies" = NULL,
                          "Settings" = settings_table("Ploidy" = 4, "Files" = c("a.txt", "b.txt"))), f)
  expect_identical(openxlsx::getSheetNames(f), c("All Records", "Empty", "Cycles - Dependencies", "Settings"))
  expect_identical(openxlsx::read.xlsx(f, sheet = "Empty")$note, "None found")
  s <- openxlsx::read.xlsx(f, sheet = "Settings")
  expect_true(all(c("Date", "Familia version", "BIGpopA version") %in% s$setting))
  expect_identical(s$value[s$setting == "Files"], "a.txt, b.txt")
})

test_that("genotypes read on upload are checked against the Ploidy at the run", {
  vcf  <- upload_lines(tetra_vcf, "tetra.vcf")
  geno <- read_genotype_upload(vcf, ploidy = NULL)   # no check on upload
  expect_equal(as.integer(attr(geno, "ploidy")), 4L)
  expect_error(check_upload_ploidy(geno, vcf, 2), "Set Ploidy to 4")
  expect_silent(check_upload_ploidy(geno, vcf, 4))
  expect_error(check_upload_ploidy(data.frame(id = "a"), list(name = "g.ped"), 4), "diploid")

  # SNMF reuses the upload instead of reading it again
  geno_path <- withr::local_tempfile(fileext = ".geno")
  write_geno_upload(vcf, geno_path, ploidy = 4, geno = geno)
  expect_identical(readLines(geno_path), c("034", "291"))
  expect_error(write_geno_upload(vcf, tempfile(), ploidy = 2, geno = geno), "Set Ploidy to 4")
})
