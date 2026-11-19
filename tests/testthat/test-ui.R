testthat::local_edition(3)

# The UI of every tab, the Help pages, the app page and server, and the
# stylesheet (R/mod_*.R, R/help_*.R, R/app_ui.R, R/app_server.R,
# inst/app/www/custom.css).

num_text    <- function(x) format(x, scientific = FALSE, digits = 15)   # as numericInput() writes it
input_value <- function(html, id) tag_attr(open_tag(html, id), "value")
is_checked  <- function(html, id) grepl("checked", open_tag(html, id), fixed = TRUE)
is_disabled <- function(html, id) grepl("shinyjs-disabled", open_tag(html, id), fixed = TRUE)
option_tag  <- function(html, value) {
  regmatches(html, regexpr(paste0('<option[^>]*value="', value, '"[^>]*>'), html, perl = TRUE))
}
has_ids <- function(html, ids) {
  for (id in ids) expect_false(is.na(open_tag(html, id)), label = id)
}

# Tab panels of the tabsets: their names (data-value) in order, and the panel
# that holds an element (the last panel opened before it)
pane_tags <- function(html) {
  m <- gregexpr('<div[^>]*class="[^"]*\\btab-pane\\b[^"]*"[^>]*>', html, perl = TRUE)
  list(start = as.integer(m[[1]]), tag = regmatches(html, m)[[1]])
}
tab_values <- function(html) vapply(pane_tags(html)$tag, tag_attr, "", name = "data-value", USE.NAMES = FALSE)
tab_of <- function(html, id) {
  at     <- regexpr(paste0('(?<![-\\w])id="', id, '"'), html, perl = TRUE)
  panes  <- pane_tags(html)
  before <- which(panes$start > 0 & panes$start < at)
  if (at < 0 || length(before) == 0) return(NA_character_)
  tag_attr(panes$tag[max(before)], "data-value")
}

#### tabs ####

test_that("the Pedigree Cleaner UI has its inputs at the test defaults and its downloads disabled", {
  html <- ui_html(mod_ped_cleaner_ui("x"))
  has_ids(html, c("x-dashboard", "x-ped_file", "x-run_check", "x-help_btn", "x-ped_results_tabs",
                  "x-results_ui", "x-summary_banner", "x-pb_ped", "x-download_examples",
                  "x-download_results", "x-download_pedigree"))
  expect_identical(tab_values(html), c("Instructions", "Data Overview", "Issue Tables"))
  expect_identical(tab_of(html, "x-dashboard"), "Data Overview")   # the tiles are not above the boxes
  expect_identical(tab_of(html, "x-results_ui"), "Issue Tables")
  expect_match(html, "The <strong>Data Overview</strong> tab opens when the pedigree is uploaded", fixed = TRUE)
  for (id in names(cleaner_defaults)) {
    expect_identical(is_checked(html, paste0("x-", id)), cleaner_defaults[[id]], label = id)
  }
  expect_true(is_disabled(html, "x-download_results"))
  expect_true(is_disabled(html, "x-download_pedigree"))
  expect_identical(tag_attr(open_tag(html, "x-ped_file"), "accept"), ".txt,.tsv,.csv")
  for (text in c("Set conflicting parents to missing (0)", "Set parents with inconsistent sex roles to missing (0)",
                 "Download Results (Excel)", "Download Cleaned Pedigree (.txt)", "Run Pedigree Check")) {
    expect_match(html, text, fixed = TRUE)
  }
})

test_that("the Validate Pedigree UI has its inputs at the test defaults and a clickable plot", {
  html <- ui_html(mod_validate_ped_ui("x"))
  has_ids(html, c("x-dashboard", "x-pedigree_file", "x-genotypes_file", "x-founders_file", "x-run_validate",
                  "x-validate_results_tabs", "x-results_ui", "x-summary_banner", "x-pb_validate", "x-error_plot",
                  "x-error_plot_hint", "x-error_plot_bar_ui", "x-download_validate_plot", "x-download_examples"))
  expect_identical(tab_values(html), c("Instructions", "Data Overview", "Issue Tables", "Mendelian Error Plot"))
  expect_identical(tab_of(html, "x-dashboard"), "Data Overview")
  expect_identical(tab_of(html, "x-error_plot"), "Mendelian Error Plot")
  expect_match(html, "The <strong>Data Overview</strong> tab opens when a file is uploaded", fixed = TRUE)
  for (id in names(validate_defaults)) {
    expect_identical(input_value(html, paste0("x-", id)), num_text(validate_defaults[[id]]), label = id)
  }
  expect_true(is_disabled(html, "x-download_validate_all"))
  expect_true(is_disabled(html, "x-download_pedigree"))
  expect_false(is_disabled(html, "x-download_validate_plot"))
  expect_identical(tag_attr(open_tag(html, "x-genotypes_file"), "accept"), paste(genotype_upload_accept, collapse = ","))

  plot <- open_tag(html, "x-error_plot")
  expect_identical(tag_attr(plot, "data-click-id"), "x-error_plot_click")
  expect_identical(tag_attr(plot, "data-hover-id"), "x-error_plot_hover")
  expect_identical(tag_attr(plot, "data-hover-delay"), "150")
  expect_identical(tag_attr(plot, "data-hover-delay-type"), "debounce")
  for (text in c("Download Results (Excel)", "Download Validated Pedigree (.txt)", "Founders File (optional)",
                 "click a bar to list its trios under the plot")) {
    expect_match(html, text, fixed = TRUE)
  }
})

test_that("the Find Parentage UI has its inputs at the test defaults and Fill in pedigree grayed out", {
  html <- ui_html(mod_find_parentage_ui("x"))
  has_ids(html, c("x-dashboard", "x-genotypes_file", "x-parents_file", "x-progeny_file", "x-founders_file",
                  "x-method", "x-run_parentage", "x-parentage_results_tabs", "x-results_ui", "x-summary_banner",
                  "x-pb_parentage", "x-error_plot", "x-error_plot_hint", "x-error_plot_bar_ui",
                  "x-download_parentage_plot", "x-download_examples"))
  expect_identical(tab_values(html), c("Instructions", "Data Overview", "Issue Tables", "Mendelian Error Plot"))
  expect_identical(tab_of(html, "x-dashboard"), "Data Overview")
  expect_identical(tab_of(html, "x-error_plot"), "Mendelian Error Plot")
  expect_match(html, "The <strong>Data Overview</strong> tab opens when a file is uploaded", fixed = TRUE)
  for (id in c("error_threshold", "min_markers", "ploidy", "top_n")) {
    expect_identical(input_value(html, paste0("x-", id)), num_text(parentage_defaults[[id]]), label = id)
  }
  for (id in c("show_ties", "allow_parent_selfing", "exclude_self_match")) {
    expect_identical(is_checked(html, paste0("x-", id)), parentage_defaults[[id]], label = id)
  }
  expect_match(option_tag(html, parentage_defaults$method), "selected", fixed = TRUE)
  expect_match(option_tag(html, "fill_pedigree"), "disabled", fixed = TRUE)
  expect_false(grepl("disabled", option_tag(html, "best_pair"), fixed = TRUE))
  expect_match(html, "data-display-if=\"input\\.method == (&#39;|')fill_pedigree(&#39;|')\"")
  expect_true(is_disabled(html, "x-download_parentage_all"))
  plot <- open_tag(html, "x-error_plot")
  expect_identical(tag_attr(plot, "data-click-id"), "x-error_plot_click")
  expect_identical(tag_attr(plot, "data-hover-id"), "x-error_plot_hover")
  expect_match(html, "Progeny / Pedigree File", fixed = TRUE)
})

test_that("the BreedTools UI has its inputs at the test defaults and accepts .csv Reference IDs", {
  html <- ui_html(mod_polybreedtools_ui("x"))
  has_ids(html, c("x-dashboard", "x-reference_file", "x-reference_map_ui", "x-ref_ids_file", "x-validation_file",
                  "x-validation_map_ui", "x-run", "x-preview", "x-bar_plot", "x-status", "x-example_ids",
                  "x-example_genos", "x-download_ids", "x-download_genos", "x-download_poly_figure",
                  "x-download_examples"))
  expect_identical(tab_values(html), c("Instructions", "Data Overview", "Results Table", "Ancestry Plot",
                                       "Reference IDs", "Genotypes"))
  expect_identical(tab_of(html, "x-dashboard"), "Data Overview")
  expect_identical(tab_of(html, "x-preview"), "Results Table")
  expect_match(html, "<strong>Data Overview:</strong> opens when a file is uploaded", fixed = TRUE)
  for (id in c("ploidy", "assign_threshold")) {
    expect_identical(input_value(html, paste0("x-", id)), num_text(breedtools_defaults[[id]]), label = id)
  }
  for (id in c("poly_show_sample_labels", "poly_sort_by_predicted")) {
    expect_identical(is_checked(html, paste0("x-", id)), breedtools_defaults[[id]], label = id)
  }
  expect_identical(tag_attr(open_tag(html, "x-poly_label_size"), "data-from"), num_text(breedtools_defaults$poly_label_size))
  expect_match(option_tag(html, breedtools_defaults$color_choice), "selected", fixed = TRUE)
  expect_true(is_disabled(html, "x-download_poly_all"))
  expect_identical(tag_attr(open_tag(html, "x-ref_ids_file"), "accept"), ".txt,.tsv,.csv")
  expect_match(html, "Reference IDs (.txt, .csv)", fixed = TRUE)
  expect_match(html, "#x-status { white-space: pre-wrap; word-break: break-word; }", fixed = TRUE)
})

test_that("the SNMF UI has its inputs at the test defaults, the example wording and the LEA banner when needed", {
  html <- ui_html(mod_SNMF_ui("x"))
  has_ids(html, c("x-dashboard", "x-snmf_file", "x-snmf_run", "x-snmf_results_tabs", "x-snmf_ce_plot",
                  "x-snmf_ce_table", "x-snmf_q_plot", "x-snmf_q_table", "x-snmf_status", "x-snmf_best_k_box",
                  "x-snmf_best_ce_box", "x-pb_snmf", "x-snmf_selectors_ui", "x-download_snmf_figure",
                  "x-download_examples"))
  expect_identical(tab_values(html), c("Instructions", "Data Overview", "Cross-Entropy", "Ancestry Plot",
                                       "Q Matrix", "Logs"))
  expect_identical(tab_of(html, "x-dashboard"), "Data Overview")
  expect_identical(tab_of(html, "x-snmf_q_table"), "Q Matrix")
  expect_match(html, "The <strong>Data Overview</strong> tab opens when the genotypes are uploaded", fixed = TRUE)
  numeric_ids <- setdiff(names(snmf_defaults), "snmf_select_mode")
  for (id in numeric_ids) {
    expect_identical(input_value(html, paste0("x-", id)), num_text(snmf_defaults[[id]]), label = id)
  }
  expect_match(html, '<input[^>]*value="auto_entropy"[^>]*checked')
  expect_true(is_disabled(html, "x-download_snmf_all"))
  expect_identical(tag_attr(open_tag(html, "x-snmf_file"), "accept"), ".vcf,.gz,.geno")
  expect_match(html, paste("The <strong>Example file</strong> link downloads <code>example_snmf.vcf</code>,",
                           "which is tetraploid (Ploidy 4). For your own file, set <strong>Ploidy</strong> to",
                           "match it."), fixed = TRUE)
  has_lea <- nzchar(system.file(package = "LEA"))
  expect_identical(grepl("SNMF needs the LEA package, which is not installed.", html, fixed = TRUE), !has_lea)
  if (!has_lea) expect_match(html, 'BiocManager::install("LEA")', fixed = TRUE)
})

test_that("the Home tab introduces the app", {
  html <- ui_html(mod_Home_ui("Home_1"))
  expect_match(html, "Familia: Pedigree Validation and Ancestry Assessment App", fixed = TRUE)
  expect_match(html, "Unsupervised Ancestry Estimation (SNMF)", fixed = TRUE)
})

#### help ####

help_pages <- list(ped_cleaner = help_content_ped_cleaner, validate_ped = help_content_validate_ped,
                   find_parentage = help_content_find_parentage, polybreedtools = help_content_polybreedtools,
                   SNMF = help_content_SNMF)

test_that("every Help page shows its tags, explains the table search and uses unique prefixed ids", {
  search_line <- paste0("<em>Search</em> box finds the text in any column, ignoring\\s+case;\\s+to search one",
                        "\\s+column,\\s+type in the box under its header")
  for (page in names(help_pages)) {
    for (prefix in c("modal", "page", "")) {
      html <- ui_html(help_pages[[page]](collapse_fn = make_collapse_panel, id_prefix = prefix))
      lab  <- paste(page, prefix)
      expect_false(grepl("&lt;/?(code|strong|em|b|br|ul|ol|li|p|sup|span|div)\\b", html, perl = TRUE), label = lab)
      expect_match(html, search_line, label = lab)
      ids <- sub('^id="([^"]*)"$', "\\1", regmatches(html, gregexpr('(?<![-\\w])id="[^"]*"', html, perl = TRUE))[[1]])
      expect_true(length(ids) > 0, label = lab)
      expect_false(anyDuplicated(ids) > 0, label = lab)
      if (nzchar(prefix)) expect_true(all(startsWith(ids, paste0(prefix, "_"))), label = lab)
      targets <- sub('^data-target="#', "", regmatches(html, gregexpr('data-target="#[^"]*', html))[[1]])
      expect_setequal(targets, ids)
    }
    # the built-in panels (no collapse_fn) give the same panels
    default <- ui_html(help_pages[[page]]())
    expect_identical(regmatches(default, gregexpr('data-target="#[^"]*"', default))[[1]],
                     regmatches(ui_html(help_pages[[page]](collapse_fn = make_collapse_panel)),
                                gregexpr('data-target="#[^"]*"',
                                         ui_html(help_pages[[page]](collapse_fn = make_collapse_panel))))[[1]])
  }
})

test_that("the Help pages show the code names as code, not as text", {
  vp <- ui_html(help_content_validate_ped())
  expect_match(vp, "<code>recommended_action</code>", fixed = TRUE)
  expect_match(vp, "<code>low_markers_remove_female_parent</code>", fixed = TRUE)
  expect_match(vp, "<code>pedigree_validated.txt</code>", fixed = TRUE)
  fp <- ui_html(help_content_find_parentage())
  expect_match(fp, "Requires <code>sex</code> column", fixed = TRUE)
  pc <- ui_html(help_content_ped_cleaner())
  expect_match(pc, "<code>pedigree_cleaned.txt</code>", fixed = TRUE)
  expect_match(pc, "<strong>Keep a conflicting trio</strong>", fixed = TRUE)
  sn <- ui_html(help_content_SNMF())
  expect_match(sn, "example_snmf.vcf, which is tetraploid (Ploidy 4)", fixed = TRUE)
  expect_match(sn, 'BiocManager::install("LEA")', fixed = TRUE)
})

test_that("the Help tab shows the help of every tab with page ids", {
  shiny::testServer(mod_help_server, {
    html <- ui_html(output$help_accordion)
    tabs <- c("ped_cleaner", "find_parentage", "validate_ped", "polybreedtools", "snmf")
    for (tab in tabs) {
      expect_match(html, paste0('id="top_', tab, '"'), fixed = TRUE)
      expect_match(html, paste0('data-target="#top_', tab, '"'), fixed = TRUE)
    }
    expect_match(html, "BreedTools<sup>poly</sup>", fixed = TRUE)
    ids <- sub('^id="([^"]*)"$', "\\1", regmatches(html, gregexpr('(?<![-\\w])id="[^"]*"', html, perl = TRUE))[[1]])
    expect_false(anyDuplicated(ids) > 0)
    expect_true(all(startsWith(setdiff(ids, paste0("top_", tabs)), "page_")))
  })
})

#### app ####

test_that("the app page has every tab and module, the logo and the version in the footer", {
  html <- ui_html(app_ui(NULL))
  for (tab in c("welcome", "ped_cleaner", "validate_ped", "find_parentage", "polybreedtools", "snmf", "help")) {
    expect_match(html, paste0('id="shiny-tab-', tab, '"'), fixed = TRUE)
  }
  has_ids(html, c("ped_cleaner_1-ped_file", "validate_ped_1-genotypes_file", "find_parentage_1-progeny_file",
                  "PolyBreedTools_1-reference_file", "SNMF_1-snmf_file", "help_1-help_accordion"))
  expect_match(html, paste0("v", utils::packageVersion("Familia")), fixed = TRUE)
  expect_match(html, 'src="www/familia_logo.png"', fixed = TRUE)
  for (button in c("session_info_button", "updates_info_button")) {   # quotes are escaped in attributes
    expect_match(html, paste0("Shiny\\.setInputValue\\((&#39;|')", button, "(&#39;|'), Math\\.random\\(\\)\\)"))
  }
  expect_true(file.exists(app_sys("app/www/familia_logo.png")))
})

test_that("the app server starts every tab, raises the upload limit and shows the session information", {
  withr::local_options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  shiny::testServer(app_server, {
    log <- record_session(session)
    expect_identical(getOption("shiny.maxRequestSize"), 1000000 * 1024^2)
    session$setInputs(session_info_button = 1)
    expect_length(log$modals, 1)
    expect_match(log$modals, "Session Information", fixed = TRUE)
    expect_match(log$modals, "R version", fixed = TRUE)
    path <- output$download_session_info
    expect_identical(basename(path), paste0("session_info_", Sys.Date(), ".txt"))
    expect_match(paste(readLines(path), collapse = "\n"), "R version", fixed = TRUE)
  })
})

test_that("Check for Updates compares version numbers, not text, and says when GitHub gives no answer", {
  skip_if(utils::packageVersion("testthat") < "3.1.7", "needs testthat::local_mocked_bindings()")
  withr::local_options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  installed <- utils::packageVersion("Familia")
  answer    <- new.env()
  answer$online <- TRUE
  answer$fail   <- FALSE
  answer$status <- 200L
  answer$tag    <- ""
  local_mocked_bindings(
    curl_fetch_memory = function(...) if (answer$online) list(status_code = 200L) else stop("offline"),
    new_handle        = function(...) NULL,
    GET               = function(...) if (answer$fail) stop("timeout") else structure(list(), class = "fake_response"),
    status_code       = function(...) answer$status,
    content           = function(...) list(tag_name = answer$tag)
  )
  # minor + 10 has two digits: newer as a version, older as text (e.g. 2.12.0 vs 2.2.0)
  newer <- paste0(installed$major, ".", installed$minor + 10L, ".0")
  shiny::testServer(app_server, {
    log  <- record_session(session)
    last <- function() log$modals[length(log$modals)]

    answer$tag <- paste0("v", newer, "-beta")   # the v and the suffix are dropped
    session$setInputs(updates_info_button = 1)
    expect_match(last(), paste("Latest version:", newer), fixed = TRUE)
    expect_match(last(), "A new version is available on GitHub!", fixed = TRUE)

    answer$tag <- as.character(installed)
    session$setInputs(updates_info_button = 2)
    expect_match(last(), paste("Installed version:", installed), fixed = TRUE)
    expect_match(last(), "Your package is up-to-date!", fixed = TRUE)

    answer$status <- 404L
    session$setInputs(updates_info_button = 3)
    expect_match(last(), "The latest release could not be retrieved from GitHub. Please try again later.", fixed = TRUE)

    answer$status <- 200L
    answer$fail   <- TRUE
    session$setInputs(updates_info_button = 4)
    expect_match(last(), "The latest release could not be retrieved from GitHub.", fixed = TRUE)

    answer$online <- FALSE
    session$setInputs(updates_info_button = 5)
    expect_match(last(), "No Internet Connection", fixed = TRUE)
    expect_match(last(), "Please check your internet connection and try again.", fixed = TRUE)
    expect_length(log$modals, 5)
  })
})

test_that("the stylesheet has the compact rows, Keep boxes, summary tiles and plot spinner", {
  css <- paste(readLines(app_sys("app/www/custom.css"), warn = FALSE), collapse = "\n")
  for (rule in c("table.dataTable tbody td {\n  padding: 3px 8px !important;",
                 "table.dataTable tbody td.keep-cell::before {\n  content: \"\\2610\";",
                 "table.dataTable tbody tr.selected td.keep-cell::before {\n  content: \"\\2611\";",
                 ".summary-strip {\n  display: grid;\n  grid-template-columns: repeat(auto-fill, minmax(130px, 1fr));",
                 ".summary-tile {", ".summary-tile-value {", ".summary-tile-label {",
                 ".summary-strip-note {", ".shiny-plot-output.recalculating::after {",
                 "animation: familia-spin 0.8s linear infinite;", "@keyframes familia-spin",
                 ".plot-hint {")) {
    expect_match(css, rule, fixed = TRUE)
  }
})
